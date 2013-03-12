package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.junit.Test;

public class EvaluatorTest extends JelispTestCase {
  @Test
  public void testEvaluateInteger () {
    LispObject LispObject = evaluateString("5");
    Assert.assertEquals(new LispInteger(5), LispObject);
  }

  @Test
  public void testEvaluateString () {
    LispObject LispObject = evaluateString("\"test\"");
    Assert.assertEquals(new LispString("test"), LispObject);
  }

  @Test (expected = VoidVariableException.class)
  public void testEvaluateSymbol() {
    evaluateString("test");
  }

  @Test
  public void testNil () {
    LispObject n = evaluateString("nil");
    Assert.assertEquals(LispSymbol.ourNil, n);
  }

  @Test
  public void testT () {
    LispObject n = evaluateString("t");
    Assert.assertEquals(LispSymbol.ourT, n);
  }

  @Test
  public void testOptionalRest() {
    try {
      evaluateString("(defun f (a &optional b) a b)");
      LispObject LispObject = evaluateString("(f 5)");
      Assert.assertEquals(LispSymbol.ourNil, LispObject);

      evaluateString("(defun f (a &optional b) b a)");
      LispObject = evaluateString("(f 5)");
      Assert.assertEquals(new LispInteger(5), LispObject);

      evaluateString("(defun f (a &optional b c &rest d e) a b c d e)");
      LispObject = evaluateString("(f 1 2 3 4 5)");
      Assert.assertEquals(LispSymbol.ourNil, LispObject);

      evaluateString("(defun f (a &optional b c &rest d e) d)");
      LispObject = evaluateString("(f 1 2 3 4 5)");
      Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(5)), LispObject);

      evaluateString("(defun f (a &optional b c) b c)");
      LispObject = evaluateString("(f 1)");
      Assert.assertEquals(LispSymbol.ourNil, LispObject);

    } catch (LispException e) {
      System.out.println(e.getMessage());
      throw e;
    }
  }

  @Test
  public void testEvalGlobalVar() {
    LispObject var = evaluateString("default-directory");
    org.junit.Assert.assertEquals(LispSymbol.ourNil, var);
  }

  @Test
  public void testFinder () {
    try {
      LispObject path = evaluateString("(find-lisp-object-file-name 'edit-abbrevs-map 'defvar)");
      Assert.assertEquals(new LispString("src/buffer.c"), path);
    } catch (Exception e) {
      System.out.println(getCauseMsg(e));
    }
  }
}
