package org.jetbrains.emacs4ij.jelisp.exception;

public class Emacs4ijFatalException extends RuntimeException {
  public Emacs4ijFatalException (String message) {
    super(message);
  }
}
