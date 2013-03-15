package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;

import java.util.List;

interface Manager<T> {
  boolean define (T item);
  @NotNull T getCurrent () ;
  T switchTo (T item);
  void bury (T item);
  List<T> getData();
  int getSize();
  void remove (T item);
  void clear();
  boolean isAlive (T item);
  boolean isEmpty();
}
