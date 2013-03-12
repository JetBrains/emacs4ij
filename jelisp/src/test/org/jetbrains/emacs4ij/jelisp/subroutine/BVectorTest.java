package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.junit.Test;

public class BVectorTest extends JelispTestCase {
  @Test
  public void testVector() {
    evaluateString("(defvar v1 '[2 3 4])");
    evaluateString("(defvar v2 (vector (aref v1 0) (aref v1 1)))");
    LispObject v = evaluateString("v2");
    Assert.assertEquals(new LispVector(new LispInteger(2), new LispInteger(3)), v);
    evaluateString("(aset v2 0 5)");
    v = evaluateString("v2");
    Assert.assertEquals(new LispVector(new LispInteger(5), new LispInteger(3)), v);
    v = evaluateString("v1");
    Assert.assertEquals(new LispVector(new LispInteger(2), new LispInteger(3), new LispInteger(4)), v);
  }

  @Test
  public void testVectorEmpty() {
    LispObject v = evaluateString("(vector)");
    Assert.assertEquals(new LispVector(), v);
  }

  @Test
  public void testMakeVector() {
    LispObject vector = evaluateString("(make-vector 3 0)");
    Assert.assertEquals(new LispVector(new LispInteger(0), new LispInteger(0), new LispInteger(0)), vector);
  }
}
