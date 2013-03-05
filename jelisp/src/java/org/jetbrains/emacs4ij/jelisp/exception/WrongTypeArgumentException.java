package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

@Error("wrong-type-argument")
public class WrongTypeArgumentException extends LispException {
  public WrongTypeArgumentException(String expectedType, String gotValue) {
    super("'(wrong-type-argument " + expectedType + " " + gotValue + ')');
  }

  public WrongTypeArgumentException(String expectedType, LispObject gotValue) {
    super("'(wrong-type-argument " + expectedType + " " + (gotValue == null ? "NULL" : gotValue.toString()) + ')');
  }
}
