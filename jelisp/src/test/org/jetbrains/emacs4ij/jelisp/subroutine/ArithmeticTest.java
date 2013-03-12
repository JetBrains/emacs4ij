package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFloat;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispNumber;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Test;

public class ArithmeticTest extends JelispTestCase {
  @Test
  public void testPlusInteger() throws LispException {
    LispObject lispObject = evaluateString("(+ 2 2)");
    Assert.assertEquals(new LispInteger(4), lispObject);
  }

  @Test
  public void testPlusFloat () {
    LispObject lispObject = evaluateString("(+ 2 2.0)");
    Assert.assertEquals(new LispFloat(4), lispObject);
  }

  @Test
  public void testPlusSimple () {
    LispNumber n = Arithmetic.plus(new LispInteger(5), new LispFloat(6.6));
    Assert.assertEquals(new LispFloat(11.6), n);
  }

  @Test
  public void testPlusMarker () {
    try {
      Arithmetic.plus(new LispMarker(), new LispMarker());
    } catch (Exception e) {
      Assert.assertEquals("Marker does not point anywhere", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testMore() {
    LispSymbol more = Arithmetic.more(new LispInteger(5), new LispFloat(1.3));
    Assert.assertEquals(LispSymbol.ourT, more);
  }

  @Test
  public void testMultiplySimple () {
    LispNumber n = Arithmetic.multiply(new LispInteger(5), new LispFloat(2.0));
    Assert.assertEquals(new LispFloat(10), n);
  }

  @Test
  public void testPlusEmpty () {
    LispObject lispObject = evaluateString("(+)");
    Assert.assertEquals(new LispInteger(0), lispObject);
  }

  @Test
  public void testMultiply() throws Exception {
    LispObject LispObject = evaluateString("(* 2 2)");
    Assert.assertEquals(new LispInteger(4), LispObject);
  }

  @Test
  public void testNumOrMarkersEqual() {
    LispObject r = evaluateString("(= 1 2)");
    Assert.assertEquals(LispSymbol.ourNil, r);
    r = evaluateString("(= 1 1.0)");
    Assert.assertEquals(LispSymbol.ourT, r);
  }

  @Test
  public void testNumOrMarkersNilMarker() {
    try {
      evaluateString("(= 1 (make-marker))");
    } catch (Exception e) {
      Assert.assertEquals("Marker does not point anywhere", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testNumOrMarkersWrongArg() {
    try {
      evaluateString("(= 1 'a)");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument number-or-marker-p a)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testNumOrMarkersNotEqual() {
    LispObject r = evaluateString("(/= 1 2)");
    Assert.assertEquals(LispSymbol.ourT, r);
    r = evaluateString("(/= 1 1.0)");
    Assert.assertEquals(LispSymbol.ourNil, r);
  }

  @Test
  public void testNumOrMarkersLessOrEqual() {
    LispObject r = evaluateString("(<= 1 2)");
    Assert.assertEquals(LispSymbol.ourT, r);
    r = evaluateString("(<= 1 1.0)");
    Assert.assertEquals(LispSymbol.ourT, r);
    r = evaluateString("(<= 2 1.0)");
    Assert.assertEquals(LispSymbol.ourNil, r);
  }

  @Test
  public void testMoreOrEqual() {
    LispSymbol more = Arithmetic.moreOrEqual(new LispInteger(5), new LispFloat(1.3));
    Assert.assertEquals(LispSymbol.ourT, more);
    more = Arithmetic.moreOrEqual(new LispInteger(5), new LispFloat(5));
    Assert.assertEquals(LispSymbol.ourT, more);
    more = Arithmetic.moreOrEqual(new LispInteger(3), new LispFloat(4));
    Assert.assertEquals(LispSymbol.ourNil, more);
  }

  @Test
  public void testMinusInteger() throws LispException {
    LispObject lispObject = evaluateString("(- 2 1)");
    Assert.assertEquals(new LispInteger(1), lispObject);
  }

  @Test
  public void testMinusOne() throws LispException {
    LispObject lispObject = evaluateString("(- 1)");
    Assert.assertEquals(new LispInteger(-1), lispObject);
  }

  @Test
  public void testMinusZero() throws LispException {
    LispObject lispObject = evaluateString("(-)");
    Assert.assertEquals(new LispInteger(0), lispObject);
  }

  @Test
  public void testMinusFloat () {
    LispObject lispObject = evaluateString("(- 2 2.0)");
    Assert.assertEquals(new LispFloat(0.0), lispObject);
  }

  @Test
  public void testMinusSimple () {
    LispNumber n = Arithmetic.minus(new LispInteger(5), new LispFloat(6.8), new LispInteger(1));
    Assert.assertEquals(new LispFloat(-2.8), n);
  }

  @Test
  public void testMinusMarker () {
    try {
      Arithmetic.minus(new LispMarker(), new LispMarker());
    } catch (Exception e) {
      Assert.assertEquals("Marker does not point anywhere", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testLogAnd() {
    LispObject r = evaluateString("(logand)");
    Assert.assertEquals(new LispInteger(-1), r);
    r = evaluateString("(logand 0)");
    Assert.assertEquals(new LispInteger(0), r);
    r = evaluateString("(logand 1)");
    Assert.assertEquals(new LispInteger(1), r);
    r = evaluateString("(logand 12 20)");
    Assert.assertEquals(new LispInteger(4), r);
  }

  @Test
  public void testLogNot() {
    LispObject r = evaluateString("(lognot 0)");
    Assert.assertEquals(new LispInteger(-1), r);
    r = evaluateString("(lognot 1)");
    Assert.assertEquals(new LispInteger(-2), r);
    r = evaluateString("(lognot -1)");
    Assert.assertEquals(new LispInteger(0), r);
  }

  @Test
  public void testLeftShift () {
    Assert.assertEquals(new LispInteger(65536), evaluateString("(lsh 1 16)"));
  }

  @Test
  public void testEql() {
    LispObject eq = evaluateString("(eql 1 1.0)");
    Assert.assertEquals(LispSymbol.ourNil, eq);
    eq = evaluateString("(eql 1.0 1.0)");
    Assert.assertEquals(LispSymbol.ourT, eq);
    eq = evaluateString("(eql (make-marker) 1.0)");
    Assert.assertEquals(LispSymbol.ourNil, eq);
    eq = evaluateString("(eql 'a 1.0)");
    Assert.assertEquals(LispSymbol.ourNil, eq);
  }

  @Test
  public void testFloor() {
    Assert.assertEquals(new LispInteger(5), evaluateString("(floor 5.36)"));
    Assert.assertEquals(new LispInteger(5), evaluateString("(floor 5.36 nil)"));
    Assert.assertEquals(new LispInteger(2), evaluateString("(floor 5.36 2.02)"));
  }

  @Test
  public void testDivide() {
    Assert.assertEquals(new LispInteger(3), evaluateString("(/ 6 2)"));
    Assert.assertEquals(new LispFloat(6.125), evaluateString("(/ 24.5 2 2 1 1 1)"));
    Assert.assertEquals(new LispInteger(1), evaluateString("(/ 5 3 1)"));
    Assert.assertEquals(new LispFloat(2), evaluateString("(/ 5 1 2.5)"));
  }

  @Test
  public void testMod() {
    Assert.assertEquals(new LispInteger(1), evaluateString("(mod 15 2)"));
    Assert.assertEquals(new LispInteger(2), evaluateString("(mod 2 15)"));
    Assert.assertEquals(new LispFloat(2.2), evaluateString("(mod 2.2 2.3)"));
    Assert.assertEquals(new LispFloat(2.1000000000000005), evaluateString("(mod 4.4 2.3)"));
  }
}
