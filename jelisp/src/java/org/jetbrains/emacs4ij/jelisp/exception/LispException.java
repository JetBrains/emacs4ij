package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * this is the base elisp exception
 */
public class LispException extends RuntimeException {
  //TODO: store the position where the exception raised
  protected StringBuilder myStackTrace;

  public LispException () {
    super (JelispBundle.message("unknown.exception"));
  }

  public LispException (String message) {
    super(message);
    if (message.contains("guts")) {
      System.out.println();
    }
  }

  public LispException(String message, StringBuilder stackTrace) {
    super(message);
    myStackTrace = stackTrace;
  }
}
