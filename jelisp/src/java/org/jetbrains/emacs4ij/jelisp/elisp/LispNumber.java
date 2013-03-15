package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * base class for elisp numbers
 */
public abstract class LispNumber<T> implements LispAtom {
  protected T myData;

  public T getData() {
    return myData;
  }

  public double getDoubleData() {
    return this instanceof LispInteger ? ((Integer)getData()).doubleValue() : (Double)getData();
  }

  @Override
  public LispObject evaluate(Environment environment) {
    return this;
  }

  @Override
  public String toString() {
    return myData.toString();
  }
}
