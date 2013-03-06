package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

public class LispThrow extends LispException {
  private LispObject myTag;
  private LispObject myValue;

  public LispThrow (LispObject tag, LispObject value) {
    myTag = tag;
    myValue = value;
  }

  public LispObject getTag() {
    return myTag;
  }

  public LispObject getValue() {
    return myValue;
  }

  @Override
  public String getMessage() {
    return JelispBundle.message("lisp.throw", myTag.toString(), myValue.toString());
  }
}
