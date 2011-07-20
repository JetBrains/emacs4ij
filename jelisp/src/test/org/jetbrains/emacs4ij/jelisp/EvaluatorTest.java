package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluatorTest {

    Environment environment = Environment.ourGlobal;

    private LispObject evaluateString (String lispCode) throws LispException {
        Parser p = new Parser();
        return Evaluator.evaluate(p.parseLine(lispCode), environment);
    }

    @Test
    public void testEvaluateInteger () {
        LispObject lispObject = Evaluator.evaluate(new LispInteger(5), environment);
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testEvaluateString () {
        LispObject lispObject = Evaluator.evaluate(new LispString("test"), environment);
        Assert.assertEquals(new LispString("test"), lispObject);
    }

    @Test
    public void testPlus() throws LispException {
        LispObject lispObject = evaluateString("(+ 2 2)");
        Assert.assertEquals(new LispInteger(4), lispObject);
    }

    @Test
    public void testInnerLists() throws LispException {
        LispObject lispObject = evaluateString("(+ 2 (+ 2 3))");
        Assert.assertEquals(new LispInteger(7), lispObject);
    }

    @Test
    public void testQuote () {
        LispObject lispObject = evaluateString("'5");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testQuotedQuotedList () {
        LispObject lispObject = evaluateString("'(quote )");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testSetVar() throws LispException {
        LispObject value = evaluateString("(set 'var 5)");
        Assert.assertEquals("set return value assertion", new LispInteger(5), value);

        LispObject lispObject = evaluateString("var");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testDefun3args() {
        LispObject fun = evaluateString("(defun mult7 (arg) (* 7 arg))");
        Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        LispObject value = evaluateString("(mult7 5)");
        Assert.assertEquals("mult7 return value assertion", new LispInteger(35), value);
    }

    @Test
    public void testDefun4args() {
        LispObject fun = evaluateString("(defun mult7 (arg) \"multiplies arg*7\" (* 7 arg))");
        Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        LispObject value = evaluateString("(mult7 5)");
        Assert.assertEquals("mult7 return value assertion", new LispInteger(35), value);
    }

    @Test (expected = WrongNumberOfArgumentsException.class)
    public void testDefunWrongNumberOfArgs() {
        LispObject fun = evaluateString("(defun mult7 () ())");
        Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        evaluateString("(mult7 5)");
    }

    @Test
    public void testDefunEmptyBody() {
        LispObject fun = evaluateString("(defun nilFun () ())");
        Assert.assertEquals("defun return value assertion", new LispSymbol("nilFun"), fun);
        LispObject value = evaluateString("(nilFun)");
        Assert.assertEquals("nilFun return value assertion", LispSymbol.ourNilSymbol, value);
    }

    @Test
    public void testNil () {
        LispObject n = evaluateString("nil");
        Assert.assertEquals(LispSymbol.ourNilSymbol, n);
    }

    @Test
    public void testT () {
        LispObject n = evaluateString("t");
        Assert.assertEquals(LispSymbol.ourTSymbol, n);
    }

}
