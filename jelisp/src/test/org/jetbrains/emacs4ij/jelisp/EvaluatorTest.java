package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Before;
import org.junit.BeforeClass;
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

    @BeforeClass
    public static void runBeforeClass() {
        TestSetup.runBeforeClass();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LispObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(environment);
    }

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

    @Ignore
    @Test
    public void testEvalGlobalVar() {
        LispObject var = evaluateString("default-directory");
        org.junit.Assert.assertEquals(LispSymbol.ourNil, var);
    }

    @Ignore
    @Test
    public void testFinder () {
        try {
            LispObject path = evaluateString("(find-lisp-object-file-name 'edit-abbrevs-map 'defvar)");
            Assert.assertEquals(new LispString("src/buffer.c"), path);
        } catch (Exception e) {
            System.out.println(TestSetup.getCause(e));
        }
    }

}
