package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

public class NullBufferDocument extends LispException {
  public NullBufferDocument (String bufferName) {
    super(JelispBundle.message("no.doc", bufferName));
  }
}
