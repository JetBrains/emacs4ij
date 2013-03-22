package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.markup.EffectType;
import com.intellij.openapi.editor.markup.HighlighterTargetArea;
import com.intellij.openapi.editor.markup.MarkupModel;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerEvent;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MessageType;
import com.intellij.openapi.ui.popup.Balloon;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.ui.awt.RelativePoint;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.ide.tool.EchoArea;
import org.jetbrains.emacs4ij.ide.tool.HelpArea;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredEditorException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.EditorWrapper;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import java.awt.*;

public class MyProjectComponent implements ProjectComponent {
  private Environment myEnvironment = null;
  private Project myProject;
  private EchoArea myEchoArea;
  private HelpArea myHelpArea;

  public MyProjectComponent(Project project) {
    myProject = project;
    IdeaBuffer.setProject(project);
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override
      public void run() {
        myEchoArea = new EchoArea(myProject);
        myHelpArea = new HelpArea(myProject);
      } });
  }

  public EchoArea getEchoArea() {
    return myEchoArea;
  }

  public HelpArea getHelpArea() {
    return myHelpArea;
  }

  private void setToolWindowsEnabled(boolean enabled) {
    myHelpArea.setToolWindowEnabled(enabled, myEnvironment);
    myEchoArea.setToolWindowEnabled(enabled);
  }

  @NotNull
  public String getComponentName() {
    return "org.jetbrains.emacs4ij.MyProjectComponent";
  }

  public void projectOpened() {
    myProject.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new FileEditorManagerListener() {
      @Override
      public void fileOpened(FileEditorManager fileEditorManager, VirtualFile virtualFile) {
        Editor editor = ((TextEditor) fileEditorManager.getSelectedEditor(virtualFile)).getEditor();
        MarkupModel m2 = editor.getMarkupModel();

//        MarkupModel m2 = DocumentMarkupModel.forDocument(editor.getDocument(), myProject, true);

        TextAttributes attr = new TextAttributes(Color.blue, Color.yellow, Color.ORANGE, EffectType.LINE_UNDERSCORE, 0);
        m2.addRangeHighlighter(0, editor.getDocument().getTextLength(), 0, attr, HighlighterTargetArea.EXACT_RANGE);

        if (myEnvironment == null)
          return;
        try {

          EditorWrapper wrapper = new IdeaEditorWrapper(editor);
          try {
            myEnvironment.getEditorBuffer(wrapper);
          } catch (UnregisteredEditorException e) {//new buffer or reopen closed
            LispBuffer existing = myEnvironment.findBuffer(virtualFile.getName());
            if (existing == null)
              new IdeaBuffer(myEnvironment, virtualFile, editor);
            else
              ((IdeaBuffer)existing).reopen(editor, virtualFile);
          } finally {
            myEnvironment.switchToWindow(wrapper, true);
          }
        } catch (DoubleBufferException e) {
          //opened 1 file in 2 or more editors.
        }
      }

      @Override
      public void fileClosed(FileEditorManager fileEditorManager, VirtualFile virtualFile) {
        if (myEnvironment == null)
          return;
        if (!myEnvironment.isSelectionManagedBySubroutine())  {
          try {
            myEnvironment.hideBuffer(virtualFile.getName());
          } catch (UnregisteredBufferException e) {
            //probably the buffer was killed from code but the "selection changed" event ate the flag :)
          }
        } else myEnvironment.setSelectionManagedBySubroutine(false);
      }

      @Override
      public void selectionChanged(FileEditorManagerEvent fileEditorManagerEvent) {
        if (myEnvironment == null)
          return;
//        if (fileEditorManagerEvent.getNewFile() == null) { //file closed
//          if (myEnvironment.getBuffersSize() != 1)
//            throw new Emacs4ijFatalException(Emacs4ijBundle.message("error.n.opened.files"));
//          return;
//        }
        if (!(myEnvironment.isSelectionManagedBySubroutine())) {
          try {
            myEnvironment.onTabSwitch(new IdeaEditorWrapper(FileEditorManager.getInstance(myProject).getSelectedTextEditor()));
//                        myEnvironment.onTabSwitch(fileEditorManagerEvent.getNewFile().getName(),
//                                FileEditorManager.getInstance(myProject).getSelectedTextEditor());
          } catch (NoBufferException | UnregisteredEditorException e) {
            //the file/editor will be opened by next event, so skip
          }
        } else
          myEnvironment.setSelectionManagedBySubroutine(false);
      }
    });

    new Task.Backgroundable(myProject, Emacs4ijBundle.message("init.task"), false) {
      public void run(@NotNull ProgressIndicator indicator) {
        indicator.setText(Emacs4ijBundle.message("init.indicator.text"));
        indicator.setFraction(0.0);
        if (EnvironmentInitializer.silentInitGlobal()) {
          initEnv();
        } else {
          setToolWindowsEnabled(false);
          ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
              JBPopupFactory.getInstance()
                  .createHtmlTextBalloonBuilder(Emacs4ijBundle.message("global.env.not.initialized.message"),
                      MessageType.WARNING, new HyperlinkListener() {
                    @Override
                    public void hyperlinkUpdate(HyperlinkEvent e) {
                      if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
                        new OpenSettings().actionPerformed(myProject);
                    }
                  })
                  .createBalloon()
                  .show(RelativePoint.getNorthEastOf(WindowManager.getInstance().getIdeFrame(myProject).getComponent()),
                      Balloon.Position.atRight);
            }
          });
        }
        indicator.setFraction(1.0);
      }
    }.queue();
  }

  public void initEnv () {
    myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    EnvironmentInitializer.initProjectEnv(myProject, myEnvironment);
    setToolWindowsEnabled(true);
  }

  public void reset () {
    setToolWindowsEnabled(false);
    myEnvironment = null;
  }

  public Environment getEnvironment() {
    return myEnvironment;
  }

  public void initComponent() {
    // TODO: insert component initialization logic here
  }

  @Override
  public void disposeComponent() {
//    myEchoArea.dispose();
//    myHelpArea.dispose();
  }

  public void projectClosed() {
    // called when project is being closed
  }
}
