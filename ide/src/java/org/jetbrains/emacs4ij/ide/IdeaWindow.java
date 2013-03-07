package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.ex.FocusChangeListener;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.NoEditorException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.EditorWrapper;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;

public final class IdeaWindow implements LispWindow {
  private final int myId;
  private final LispBuffer myBuffer;
  private final LispFrame myFrame;
  private Editor myEditor;

  IdeaWindow (int id, final LispBuffer buffer, final LispFrame frame, final Editor editor) {
    myId = id;
    myBuffer = buffer;
    myFrame = frame;
    setEditor(editor);
  }

  private void setEditor (@Nullable final Editor editor) {
    myEditor = editor;
    if (editor == null)
      return;
    ((EditorEx) myEditor).addFocusListener(new FocusChangeListener() {
      @Override
      public void focusGained(Editor editor) {
        if (myBuffer != null)
          GlobalEnvironment.INSTANCE.switchToBuffer(myBuffer);
      }

      @Override
      public void focusLost(Editor editor) {
      }
    });
  }

  public Editor getEditor() {
    return myEditor;
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
    FileEditorManager fileEditorManager = FileEditorManager.getInstance(IdeaBuffer.getProject());
    VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
    for (VirtualFile file: openedFiles) {
      FileEditor[] editors = fileEditorManager.getAllEditors(file);
      for (FileEditor fileEditor: editors) {
        if (((TextEditor)fileEditor).getEditor() == myEditor) {
          Editor opened = fileEditorManager
              .openTextEditor(new OpenFileDescriptor(IdeaBuffer.getProject(), file), true);
          assert opened == myEditor;
          return;
        }
      }
    }
    throw new NoEditorException();
  }

  @Override
  public String toString() {
    return "#<window " + myId + " on " + myBuffer.getName() + '>';
  }

  @Override
  public LispObject evaluate(Environment environment) {
    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    IdeaWindow that = (IdeaWindow) o;

    if (myEditor != null ? !myEditor.equals(that.myEditor) : that.myEditor != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    return myEditor != null ? myEditor.hashCode() : 0;
  }

  @Override
  public void close() {
    myEditor = null;
    closeTab();
  }

  @Override
  public boolean isVisible() {
    return myEditor != null;
  }

  @Override
  public void open (@NotNull EditorWrapper wrapper) {
    assert myEditor == null;
    setEditor(((IdeaEditorWrapper)wrapper).getEditor());
  }

  @Override
  public void closeTab() {
    if (myBuffer instanceof IdeaBuffer) {
      VirtualFile file = ((IdeaBuffer) myBuffer).getFile();
      if (file == null)
        return;
      FileEditorManager fileEditorManager = FileEditorManager.getInstance(IdeaBuffer.getProject());
      fileEditorManager.closeFile(file);
    }
  }

  @Override
  public boolean editorEquals(EditorWrapper editor) {
    return myEditor == ((IdeaEditorWrapper)editor).getEditor();
  }

  @Override
  public Integer getDisplayStart() {
    //todo
    return 1;
  }
}
