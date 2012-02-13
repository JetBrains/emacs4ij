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

    private CustomEnvironment environment;

    @Before
    public void setUp() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs 23.2a/emacs-23.2");
        GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.2");
        GlobalEnvironment.initialize(null, null);
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
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
            Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(5)), LObject);

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
    public void testEvalGlobalVar() {
        LObject var = evaluateString("default-directory");
        org.junit.Assert.assertEquals(LispSymbol.ourNil, var);
    }

    @Ignore
    @Test
    public void testFinder () throws Throwable {
        try {
            LObject path = evaluateString("(find-lisp-object-file-name 'edit-abbrevs-map 'defvar)");
            Assert.assertEquals(new LispString("src/buffer.c"), path);
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
            throw getCause(e);
        }
    }

    @Test
    public void testFindMark () throws Throwable {
        try {
            GlobalEnvironment.getDefFromFile(GlobalEnvironment.getEmacsSource() + "/lisp/simple.el", "mark");
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
            throw getCause(e);
        }
    }

}
