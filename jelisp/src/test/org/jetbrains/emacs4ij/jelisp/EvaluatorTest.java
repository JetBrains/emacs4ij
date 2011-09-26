package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluatorTest {

    private Environment environment;

    @Before
    public void setUp() {
        Environment.ourEmacsPath = "/usr/share/emacs/23.2";
        environment = new Environment(new Environment());
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
    }

    @Test
    public void testEvaluateInteger () {
        LObject LObject = evaluateString("5");
        Assert.assertEquals(new LispInteger(5), LObject);
    }

    @Test
    public void testEvaluateString () {
        LObject LObject = evaluateString("\"test\"");
        Assert.assertEquals(new LispString("test"), LObject);
    }

    @Test (expected = VoidVariableException.class)
    public void testEvaluateSymbol() {
        evaluateString("test");
    }

    @Test
    public void testNil () {
        LObject n = evaluateString("nil");
        Assert.assertEquals(LispSymbol.ourNil, n);
    }

    @Test
    public void testT () {
        LObject n = evaluateString("t");
        Assert.assertEquals(LispSymbol.ourT, n);
    }

    @Test
    public void testOptionalRest() {
        try {
            evaluateString("(defun f (a &optional b) a b)");
            LObject LObject = evaluateString("(f 5)");
            Assert.assertEquals(LispSymbol.ourNil, LObject);

            evaluateString("(defun f (a &optional b) b a)");
            LObject = evaluateString("(f 5)");
            Assert.assertEquals(new LispInteger(5), LObject);

            evaluateString("(defun f (a &optional b c &rest d e) a b c d e)");
            LObject = evaluateString("(f 1 2 3 4 5)");
            Assert.assertEquals(LispSymbol.ourNil, LObject);

            evaluateString("(defun f (a &optional b c &rest d e) d)");
            LObject = evaluateString("(f 1 2 3 4 5)");
            Assert.assertEquals(new LispList(new LispInteger(4), new LispInteger(5)), LObject);

            evaluateString("(defun f (a &optional b c) b c)");
            LObject = evaluateString("(f 1)");
            Assert.assertEquals(LispSymbol.ourNil, LObject);

        } catch (LispException e) {
            System.out.println(e.getMessage());
            throw e;
        }
    }

    @Ignore
    @Test
    public void testFinder () {
        try {
        LObject path = evaluateString("(find-lisp-object-file-name 'edit-abbrevs-map 'defvar)");
        Assert.assertEquals(new LispString("src/buffer.c"), path);
        } catch (LispException e) {
            System.out.println(e.getMessage());
            throw e;
        }
    }
}
