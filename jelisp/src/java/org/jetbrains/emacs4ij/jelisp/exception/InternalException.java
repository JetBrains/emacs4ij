package org.jetbrains.emacs4ij.jelisp.exception;

public class InternalException extends RuntimeException {
  public InternalException(String message) {
    super(message);
  }

  public InternalException() {
    super();
  }
}
