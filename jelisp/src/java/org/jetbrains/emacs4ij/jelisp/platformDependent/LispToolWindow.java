package org.jetbrains.emacs4ij.jelisp.platformDependent;

public interface LispToolWindow extends LispWindow {
  void setId(int id);
  void setFrame(LispFrame frame);
  boolean isRegistered();
  EditorWrapper getEditor();
}
