package org.jetbrains.emacs4ij.ide;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBufferFactory;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;

public class BufferCreator implements LispBufferFactory {
  @Override
  public LispBuffer createBuffer(Environment environment, String name, String defaultDir, LispToolWindow window) {
    return new IdeaBuffer(environment, name, defaultDir, window);
  }
}
