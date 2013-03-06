package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.AssignNullDocument;
import org.jetbrains.emacs4ij.jelisp.exception.BufferOpenException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.NullBufferDocument;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;

public class IdeaBuffer extends LispBuffer {
  protected static Project ourProject;
  protected Document myDocument;
  private VirtualFile myVirtualFile;

  protected final DocumentListener myDocumentListener = new DocumentListener() {
    private int myOldPosition;
    @Override
    public void beforeDocumentChange(DocumentEvent documentEvent) {
      myOldPosition = point();
    }

    @Override
    public void documentChanged(DocumentEvent documentEvent) {
      int shift = documentEvent.getNewLength() - documentEvent.getOldLength();
      if (shift < 0) {   //delete
        updateMarkersPositions(point(), shift, false);
        return;
      }
      if (shift > 0) { //insert
        updateMarkersPositions(myOldPosition, shift, false);
      }
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

  public IdeaBuffer (Environment environment, String name, String defaultDir, LispToolWindow window) {
    this(name, environment, null, null);
    openToolBuffer(window);
    setLocalVariable("default-directory", new LispString(defaultDir));
  }

  private void openToolBuffer (LispToolWindow window) {
    getEnvironment().onToolBufferOpened(window);
    onOpen(((IdeaEditorWrapper) window.getEditor()).getEditor().getDocument());
  }

  protected final void openStandardBuffer(Editor editor) {
    getEnvironment().onBufferOpened(this, new IdeaEditorWrapper(editor));
    onOpen(editor.getDocument());
  }

  private void switchToWindow (final Editor editor, boolean switchBuffer) {
    getEnvironment().switchToWindow(new IdeaEditorWrapper(editor), switchBuffer);
  }

  @Nullable
  public VirtualFile getFile() {
    return myVirtualFile;
  }

  private void onOpen(Document document) {
    if (myDocument != null && myDocument != document)
      throw new BufferOpenException(getName());
    if (document == null)
      throw new AssignNullDocument(getName());
    if (myDocument != null)
      return;
    myDocument = document;
    myDocument.addDocumentListener(myDocumentListener);
  }

  public void reopen (Editor editor, VirtualFile file) {
    assert file == myVirtualFile;
    openStandardBuffer(editor);
  }

  @NotNull
  protected Document getDocument() {
    if (myDocument == null)
      throw new NullBufferDocument(getName());
    return myDocument;
  }

  protected Editor getEditor() {
    return ((IdeaWindow)getEnvironment().getBufferLastSelectedWindow(this)).getEditor();
  }

  @Override
  public int size() {
    return getDocument().getTextLength();
  }

  @Override
  public int point() {
    Editor editor = getEditor();
    return editor.logicalPositionToOffset(editor.getCaretModel().getLogicalPosition()) + 1;
  }

  @Override
  public String getText() {
    return getDocument().getText();
  }

  @Override
  public void setText(String text) {
    getDocument().setText(text);
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

  protected void write (final String text) {
    UIUtil.invokeLaterIfNeeded(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            getDocument().setText(text);
            gotoChar(pointMax());
          }
        });
      }
    });
  }

  @Override
  protected void insertAt (final int position, final String insertion) {
    UIUtil.invokeLaterIfNeeded(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            getDocument().insertString(position, insertion);
          }
        });
      }
    });
  }

  @Override
  public void replace(final int from, final int to, final String text) {
    UIUtil.invokeLaterIfNeeded(new Runnable() {
      @Override
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          @Override
          public void run() {
            getDocument().replaceString(from - 1, to - 1, text);
            gotoChar(from + text.length());
          }
        });
      }
    });
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
    getDocument().removeDocumentListener(myDocumentListener);
    myDocument = null;
  }

  public void closeHeader () {
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
