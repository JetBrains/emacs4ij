package org.jetbrains.emacs4ij.jelisp.exception;

@Error("void-function")
public class VoidFunctionException extends LispException {
  public VoidFunctionException (String functionName) {
    super("'(void-function " + functionName + ')');
  }
}
