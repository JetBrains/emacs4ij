package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;

public final class IdeaBuffer extends LispBuffer {
  private static Project ourProject;
  private VirtualFile myVirtualFile;
  private DocumentHolder myDocumentHolder = new DocumentHolder() {
    @Override
    protected void afterReplace(int newFinishPosition) {
      gotoChar(newFinishPosition);
    }
    @Override
    protected String getOwnerName() {
      return getName();
    }
    @Override
    protected void updateMarkersPositions(int point, int shift, boolean b) {
      IdeaBuffer.this.updateMarkersPositions(point, shift, b);
    }
    @Override
    protected int point() {
      return IdeaBuffer.this.point();
    }
  };

  protected IdeaBuffer (String name, Environment environment, @Nullable Editor editor, @Nullable VirtualFile file) {
    super(name, environment);
    myVirtualFile = file;
    if (editor != null) {
      openStandardBuffer(editor);
    }
  }

  public IdeaBuffer(Environment environment, FileEditorManager fileEditorManager, VirtualFile file) {
    this(file.getName(), environment, null, file);
    setLocalVariable("default-directory", new LispString(file.getParent().getPath() + '/'));
    for (FileEditor fileEditor: fileEditorManager.getAllEditors(file)) {
      openStandardBuffer(((TextEditor) fileEditor).getEditor());
    }
    FileEditor selected = fileEditorManager.getSelectedEditor(file);
    if (selected != null)
      switchToWindow(((TextEditor) selected).getEditor(), true);
  }

  public IdeaBuffer(Environment environment, @NotNull VirtualFile file, @Nullable Editor editor) {
    this(file.getName(), environment, editor, file);
    setLocalVariable("default-directory", new LispString(file.getParent().getPath() + '/'));
  }

  public IdeaBuffer(Environment environment, String name, String defaultDir, LispToolWindow window) {
    this(name, environment, null, null);
    openToolBuffer(window);
    setLocalVariable("default-directory", new LispString(defaultDir));
  }

  private void openToolBuffer (LispToolWindow window) {
    getEnvironment().onToolBufferOpened(window);
    myDocumentHolder.onOpen(((IdeaEditorWrapper) window.getEditor()).getEditor().getDocument());
  }

  private void openStandardBuffer(Editor editor) {
    getEnvironment().onBufferOpened(this, new IdeaEditorWrapper(editor));
    myDocumentHolder.onOpen(editor.getDocument());
  }

  private void switchToWindow (final Editor editor, boolean switchBuffer) {
    getEnvironment().switchToWindow(new IdeaEditorWrapper(editor), switchBuffer);
  }

  @Nullable
  public VirtualFile getFile() {
    return myVirtualFile;
  }

  public void reopen (Editor editor, VirtualFile file) {
    assert file == myVirtualFile;
    openStandardBuffer(editor);
  }

  public Editor getEditor() {
    return ((IdeaWindow)getEnvironment().getBufferLastSelectedWindow(this)).getEditor();
  }

  @Override
  public int size() {
    return myDocumentHolder.size();
  }

  @Override
  public int point() {
    Editor editor = getEditor();
    if (editor == null) {
      throw new IllegalStateException("null buffer editor: " + getName());
    }
    return editor.logicalPositionToOffset(editor.getCaretModel().getLogicalPosition()) + 1;
  }

  @Override
  public String getText() {
    return myDocumentHolder.getText();
  }

  @Override
  public void setText(String text) {
    myDocumentHolder.setText(text);
  }

  @Override
  protected int getLine() {
    return getEditor().getCaretModel().getVisualPosition().getLine();
  }

  @Override
  protected int getColumn() {
    return getEditor().getCaretModel().getVisualPosition().getColumn() - 1;
  }

  @Override
  public void setPoint(int position) {
    getEditor().getCaretModel().moveToOffset(position - 1);
  }

  @Override
  protected void insertAt (final int position, final String insertion) {
    myDocumentHolder.insertAt(position, insertion);
  }

  @Override
  public void replace(final int from, final int to, final String text) {
    myDocumentHolder.replace(from, to, text);
  }

  @Override
  public void setActive() {
    super.setActive();

    FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
    VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
    for (VirtualFile file: openedFiles) {
      if (file.getName().equals(getName())) {
        switchToWindow(fileEditorManager.openTextEditor(new OpenFileDescriptor(ourProject, file), true), false);
        return;
      }
    }
    //it is not in opened files, open it.
    assert myVirtualFile != null;
    Editor editor = ((TextEditor)fileEditorManager.openFile(myVirtualFile, true)[0]).getEditor();
    openStandardBuffer(editor);
    switchToWindow(editor, false);
  }

  @Override
  public void kill() {
    FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
    VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
    for (VirtualFile file: openedFiles) {
      if (file.getName().equals(getName())) {
        fileEditorManager.closeFile(file);
      }
    }
    myDocumentHolder.onKill();
  }

  @Override
  public void closeHeader() {
    try {
      Editor editor = getEditor();
      if (editor == null)
        return;
      if (editor.getHeaderComponent() == null)
        return;
      editor.setHeaderComponent(null);
    } catch (LispException e) {
      //the buffer was killed, skip
    }
  }

  public static void setProject(Project project) {
    ourProject = project;
  }

  public static Project getProject() {
    return ourProject;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof IdeaBuffer)) return false;
    if (!super.equals(o)) return false;

    IdeaBuffer that = (IdeaBuffer) o;

    if (myVirtualFile != null ? !myVirtualFile.equals(that.myVirtualFile) : that.myVirtualFile != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = super.hashCode();
    result = 31 * result + (myVirtualFile != null ? myVirtualFile.hashCode() : 0);
    return result;
  }
}
