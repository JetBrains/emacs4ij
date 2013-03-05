package org.jetbrains.emacs4ij.jelisp.platformDependent;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

public interface LispWindow extends LispObject {
  LispFrame getFrame();
  LispBuffer getBuffer();
  void setActive();
  boolean isVisible();
  void close();
  void open (@NotNull EditorWrapper editor);
  void closeTab();
  boolean editorEquals (EditorWrapper editor);

  /**
   * @return the position in displayed buffer, at which the visible part of buffer starts and NULL if none
   */
  @Nullable
  Integer getDisplayStart();
}
