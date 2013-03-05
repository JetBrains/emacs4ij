package org.jetbrains.emacs4ij.ide.tool;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.ex.FocusChangeListener;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.ide.Emacs4ijBundle;
import org.jetbrains.emacs4ij.ide.IdeaEditorWrapper;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.EditorWrapper;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;
import org.jetbrains.emacs4ij.jelisp.subroutine.Buffer;


public class HelpArea extends Emacs4ijToolWindow implements LispToolWindow {
  private int myId;
  private LispBuffer myBuffer = null;
  private LispFrame myFrame = null;

  @Override
  protected String getToolWindowName() {
    return "Emacs4ij Help";
  }

  public HelpArea (final Project project) {
    super(project);

    ((EditorEx) myEditor).addFocusListener(new FocusChangeListener() {
      @Override
      public void focusGained(Editor editor) {
        GlobalEnvironment.INSTANCE.switchToWindow(new IdeaEditorWrapper(myEditor), true);
      }

      @Override
      public void focusLost(Editor editor) {
      }
    });
  }

  public void setToolWindowEnabled (final boolean state, final Environment environment) {
    if (state && myBuffer == null) {
      myBuffer = Buffer.getToolBufferCreate(environment, Emacs4ijBundle.message("help.buffer.name"), HelpArea.this);
    }
    setToolWindowEnabled(state);
  }

  @Override
  public void dispose() {
    if (myBuffer != null) {
      myBuffer.getEnvironment().killBuffer(myBuffer);
      myBuffer = null;
    }
    super.dispose();
  }

  @Override
  public EditorWrapper getEditor() {
    return new IdeaEditorWrapper(myEditor);
  }

  @Override
  public LispFrame getFrame() {
    return myFrame;
  }

  @Override
  public LispBuffer getBuffer() {
    return myBuffer;
  }

  @Override
  public void setActive() {
    if (myBuffer == null)
      throw new NoBufferException("*Help*");
    open();
  }

  @Override
  public void close() {
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override
      public void run() {
        getIdeaToolWindow().hide(null);
      }
    });
  }

  @Override
  public void open(@NotNull EditorWrapper wrapper) {
    throw new UnsupportedOperationException(Emacs4ijBundle.message("help.area.open.with.window"));
  }

  private void open() {
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override
      public void run() {
        getIdeaToolWindow().show(null);
      }
    });
  }

  @Override
  public void closeTab() {
    close();
  }

  @Override
  public boolean editorEquals(EditorWrapper editor) {
    return myEditor == ((IdeaEditorWrapper)editor).getEditor();
  }

  @Override
  public Integer getDisplayStart() {
    return 1;
  }

  @Override
  public LispObject evaluate(Environment environment) {
    return this;
  }

  @Override
  public void setId(int id) {
    myId = id;
  }

  @Override
  public void setFrame(@NotNull LispFrame frame) {
    myFrame = frame;
  }

  @Override
  public boolean isRegistered() {
    return myFrame != null;
  }

  @Override
  public String toString() {
    return "#<window " + myId + " on " + Emacs4ijBundle.message("help.buffer.name") + '>';
  }
}
