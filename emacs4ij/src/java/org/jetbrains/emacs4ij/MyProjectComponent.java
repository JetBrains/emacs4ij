package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerEvent;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.keymap.impl.KeymapManagerImpl;
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
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.EnvironmentException;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/5/11
 * Time: 12:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class MyProjectComponent implements ProjectComponent {
    private CustomEnvironment myEnvironment = null;
    private Project myProject;

    public MyProjectComponent(Project project) {
        myProject = project;
        IdeaBuffer.setProject(project);
    }

    @NotNull
    public String getComponentName() {
        return "org.jetbrains.emacs4ij.MyProjectComponent";
    }

    public void projectOpened() {
        myProject.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, new FileEditorManagerListener() {
            @Override
            public void fileOpened(FileEditorManager fileEditorManager, VirtualFile virtualFile) {
                if (myEnvironment == null)
                    return;
                try {
                    new IdeaBuffer(myEnvironment, virtualFile.getName(), virtualFile.getParent().getPath() + '/', fileEditorManager.getSelectedTextEditor());
                } catch (DoubleBufferException e) {
                    System.err.println(e.getMessage());
                }
            }

            @Override
            public void fileClosed(FileEditorManager fileEditorManager, VirtualFile virtualFile) {
                if (myEnvironment == null)
                    return;
                if (!(myEnvironment.isSelectionManagedBySubroutine()))
                    myEnvironment.killBuffer(virtualFile.getName());
                else myEnvironment.setSelectionManagedBySubroutine(false);
            }

            @Override
            public void selectionChanged(FileEditorManagerEvent fileEditorManagerEvent) {
                if (myEnvironment == null)
                    return;

                if (fileEditorManagerEvent.getNewFile() == null) {
                    if (myEnvironment.getBuffersSize() != 1)
                        throw new Emacs4ijFatalException("The number of opened buffers doesn't correspond to number of opened files!");
                    return;
                }
                try {
                    if (!(myEnvironment.isSelectionManagedBySubroutine()))
                        myEnvironment.switchToBuffer(fileEditorManagerEvent.getNewFile().getName());
                    else myEnvironment.setSelectionManagedBySubroutine(false);
                } catch (EnvironmentException e) {
                    //ignore
                }
            }
        });

        new Task.Backgroundable(myProject, Emacs4ijBundle.message("init.task"), false) {
            public void run(@NotNull ProgressIndicator indicator) {
                indicator.setText(Emacs4ijBundle.message("init.indicator.text"));
                indicator.setFraction(0.0);
                if (EnvironmentInitializer.silentInitGlobal()) {
                    myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
                    EnvironmentInitializer.initProjectEnv(myProject, myEnvironment);
                } else {
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
                indicator.setFraction(1.0);
            }
        }.queue();
    }

    public void initEnv () {
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        EnvironmentInitializer.initProjectEnv(myProject, myEnvironment);

        LispKeymap globalKeymap = new IdeaKeymap();
        globalKeymap.defineKey("backward-char", new LispString("a"));
        ((KeymapManagerImpl)KeymapManager.getInstance()).setActiveKeymap(globalKeymap.getKeymap());
    }

    public CustomEnvironment getEnvironment() {
        return myEnvironment;
    }

    public void initComponent() {
        // TODO: insert component initialization logic here
    }

    public void disposeComponent() {
        // TODO: insert component disposal logic here
    }

    public void projectClosed() {
        // called when project is being closed
    }
}
