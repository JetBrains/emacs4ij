package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

public class CyclicDefinitionLoadException extends LispException {
  public CyclicDefinitionLoadException (String id) {
    super(JelispBundle.message("cyclic.def.load", id));
  }
}
