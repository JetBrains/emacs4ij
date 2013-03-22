package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.ui.EditorTextField;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;

public class TestIdeaMinibuffer extends LispMinibuffer {
  private static TestIdeaMinibuffer myInstance;
  private Integer myCharCode = null;

  private TestIdeaMinibuffer (int number, Editor editor, Environment environment, LispBuffer parent) {
    super(" *Minibuf-" + number + '*', environment, parent);
    setMark(new LispMarker(1, this));
    if (editor != null) {
      openStandardBuffer(editor);
    }
  }

  public static void init (@Nullable Editor editor, @Nullable Environment environment) {
    if (myInstance == null) {
      myInstance = new TestIdeaMinibuffer(0, editor, environment == null ? GlobalEnvironment.INSTANCE : environment, null);
    }
  }

  public static TestIdeaMinibuffer getInstance() {
    return myInstance;
  }

  private void openStandardBuffer(Editor editor) {
    getEnvironment().onBufferOpened(this, new IdeaEditorWrapper(editor));
  }

  @Override
  protected Integer getCharCode() {
    return myCharCode;
  }

  @Override
  protected void resetCharCode() {
    myCharCode = null;
  }

  @Override
  public int size() {
    return 0;
  }

  @Override
  public void closeHeader() {
    throw new UnsupportedOperationException();
  }

  @Override
  protected void cancelNoMatchMessageUpdate() {
  }

  @Override
  public void setCharListener() {
  }

  @Override
  protected void clearNoMatch () {
  }

  @Override
  protected void open(LispBuffer parent) {
    super.open(parent);

    final EditorTextField input = new EditorTextField("", IdeaBuffer.getProject(), FileTypes.PLAIN_TEXT);

    if (parent != null) {
      ((IdeaBuffer)parent).getEditor().setHeaderComponent(input);
    }

    input.setEnabled(true);
    Editor editor = input.getEditor();

    setActive();
    openStandardBuffer(editor);
  }

  @Override
  protected void write (final String text) {
  }

  //for test
  public void appendText (String text) {
    write(getText() + text);
  }

  @Override
  public void setActive() {
    if (getEnvironment().findBuffer(getName()) == null) throw new UnregisteredBufferException(getName());
  }

  @Override
  public String getText() {
    throw new UnsupportedOperationException();
  }

  @Override
  public void setText(String text) {
  }

  @Override
  protected void insertAt(int position, String insertion) {
  }

  @Override
  public void replace(int from, int to, String text) {
  }

  @Override
  public int point() {
    return 1;
  }

  @Override
  public void setPoint(int position) {
  }

  @Override
  public void goTo(int line, int column) {
  }

  @Override
  public int getLine() {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getColumn() {
    throw new UnsupportedOperationException();
  }

  @Override
  public int getLineStartIndex() {
    return 1;
  }
}
