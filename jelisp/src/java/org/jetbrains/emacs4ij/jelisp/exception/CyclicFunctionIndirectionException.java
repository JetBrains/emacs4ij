package org.jetbrains.emacs4ij.jelisp.exception;

@Error("cyclic-function-indirection")
public class CyclicFunctionIndirectionException extends LispException {
  public CyclicFunctionIndirectionException (String message) {
    super("'(cyclic-function-indirection " + message + ')');
  }
}
