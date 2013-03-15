package org.jetbrains.emacs4ij.jelisp.elisp.text;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;

public class Range implements Comparable<Range> {
  private int myStart;
  private int myEnd;

  public Range(int leftBound, int rightBound, LispBuffer buffer) {
    this(leftBound, rightBound, buffer.pointMin(), buffer.pointMax());
  }

  public Range(int leftBound, int rightBound, int min, int max) {
    myStart = verify(leftBound, min, max);
    myEnd   = verify(rightBound, min, max);
    swapIfNeeded();
  }

  public Range(int leftBound, int rightBound) {
    myStart = leftBound;
    myEnd = rightBound;
    swapIfNeeded();
  }

  public Range(Range range) {
    myStart = range.myStart;
    myEnd = range.myEnd;
  }

  private void swapIfNeeded() {
    if (myStart > myEnd) {
      int tmp = myStart;
      myStart = myEnd;
      myEnd = tmp;
    }
  }

  private static int verify (int what, int min, int max) {
    if (what < min || what > max) throw new ArgumentOutOfRange(what);
    return what;
  }

  public void set (Range range) {
    myStart = range.myStart;
    myEnd = range.myEnd;
  }

  public void setStart (int start) {
    if (start >= myEnd)
      throw new InternalException(JelispBundle.message("invalid.range", start, myEnd));
    myStart = start;
  }

  public void setEnd (int end) {
    if (myStart >= end)
      throw new InternalException(JelispBundle.message("invalid.range", myStart, end));
    myEnd = end;
  }

  public int getStart() {
    return myStart;
  }

  public int getEnd() {
    return myEnd;
  }

  public boolean isEmpty () {
    return myStart >= myEnd;
  }

  public boolean equals (int start, int end) {
    return myStart == start && myEnd == end;
  }

  @Override
  public String toString() {
    return myStart + " " + myEnd;
  }

  @Override
  public int compareTo(Range o) {
    return myStart < o.myStart ? -1 : myStart == o.myStart ? 0 : 1;
  }

  public boolean contains (int index) {
    return index >= myStart && index < myEnd;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof Range)) return false;

    Range range = (Range) o;

    if (myEnd != range.myEnd) return false;
    if (myStart != range.myStart) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = myStart;
    result = 31 * result + myEnd;
    return result;
  }
}
