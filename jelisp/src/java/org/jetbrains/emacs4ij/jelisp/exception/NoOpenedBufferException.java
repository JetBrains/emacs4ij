package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

public class NoOpenedBufferException extends LispException {
  public NoOpenedBufferException() {
    super(JelispBundle.message("no.opened", "buffer"));
  }
}
