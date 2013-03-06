package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

public interface LispObject {
  LispObject evaluate(Environment environment);
}
