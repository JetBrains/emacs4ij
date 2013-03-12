package org.jetbrains.emacs4ij.ide;

import org.jetbrains.emacs4ij.jelisp.platformDependent.EditorWrapper;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindowFactory;

public class WindowCreator implements LispWindowFactory {
  @Override
  public LispWindow createWindow(int id, final LispBuffer buffer, final LispFrame frame, final EditorWrapper editor) {
    return new IdeaWindow(id, buffer, frame, ((IdeaEditorWrapper)editor).getEditor());
  }
}
