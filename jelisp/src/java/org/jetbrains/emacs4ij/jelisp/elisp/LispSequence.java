package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.List;

public interface LispSequence extends LispObject {
  int size();
  List<LispObject> toLispObjectList();
  List<LispObject> mapCar (Environment environment, LispObject method);
  LispObject copy();
  String toCharString();
  boolean isEmpty();
  LispObject delete (LispObject element);
}
