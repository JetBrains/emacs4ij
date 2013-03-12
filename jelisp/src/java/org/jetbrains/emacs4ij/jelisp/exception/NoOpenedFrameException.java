package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

public class NoOpenedFrameException extends LispException {
  public NoOpenedFrameException() {
    super(JelispBundle.message("no.opened", "frame"));
  }
}