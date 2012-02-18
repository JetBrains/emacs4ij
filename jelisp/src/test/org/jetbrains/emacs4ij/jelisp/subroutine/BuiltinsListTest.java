package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsListTest {
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

    private LObject evaluateString (String lispCode) throws LispException {
        ForwardParser forwardParser = new ForwardParser();
        return forwardParser.parseLine(lispCode).evaluate(environment);
    }

    @Test
    public void testInnerLists() throws LispException {
        LObject LObject = evaluateString("(+ 2 (+ 2 3))");
        Assert.assertEquals(new LispInteger(7), LObject);
    }

    @Test
    public void testCar() {
        evaluateString("(set 'p '(1 2 3))");
        LObject LObject = evaluateString("(car p)");
        Assert.assertEquals(new LispInteger(1), LObject);
        evaluateString("(set 'p '())");
        LObject = evaluateString("(car p)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }
    
    @Test
    public void testCarNil() {
        LObject res = evaluateString("(car nil)");
        Assert.assertEquals(LispSymbol.ourNil, res);
    }

    @Test
    public void testCarWrongArg() {
        try {
            evaluateString("(set 'p 'defun)");
            evaluateString("(car p)");
        } catch (Exception e) {
            Assert.assertTrue(TestSetup.getCause(e) instanceof WrongTypeArgumentException);
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCdr() {
        evaluateString("(set 'p '(1 2 3))");
        LObject LObject = evaluateString("(cdr p)");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), LObject);
        evaluateString("(set 'p '(1))");
        LObject = evaluateString("(cdr p)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testCdrWrongArg() {
        try {
            evaluateString("(set 'p 'defun)");
            evaluateString("(cdr p)");
        } catch (Exception e) {
            Assert.assertTrue(TestSetup.getCause(e) instanceof WrongTypeArgumentException);
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCdrNil() {
        LObject res = evaluateString("(cdr nil)");
        Assert.assertEquals(LispSymbol.ourNil, res);
    }

    @Test
    public void testCarSafe() {
        evaluateString("(set 'p 'defun)");
        LObject LObject = evaluateString("(car-safe p)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testCdrSafe() throws Exception {
        evaluateString("(set 'p 'defun)");
        LObject LObject = evaluateString("(cdr-safe p)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testMemq () {
        evaluateString("(set 'a '(1 2 3))");
        LObject LObject = evaluateString("(memq 4 a)");
        Assert.assertEquals("not exist", LispSymbol.ourNil, LObject);
        LObject = evaluateString("(memq 3 a)");
        Assert.assertEquals(LispList.list(new LispInteger(3)), LObject);
    }

    @Test
    public void testMemqCons() {
        try {
            evaluateString("(setq x (cons 1 (cons (cons 5 6) 3)))");
            evaluateString("(memq 6 x)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 3)", TestSetup.getCause(e).getMessage());
        }
    }

    @Test
    public void testList() {
        LObject LObject = evaluateString("(list)");
        Assert.assertEquals("no args", LispSymbol.ourNil, LObject);
        LObject = evaluateString("(list 5 \"test\")");
        Assert.assertEquals("2 args", LispList.list(new LispInteger(5), new LispString("test")), LObject);
        LObject = evaluateString("(list nil)");
        Assert.assertEquals("list of nil -1", LispList.list(LispSymbol.ourNil), LObject);
        LObject = evaluateString("(list (list))");
        Assert.assertEquals("list of nil -2", LispList.list(LispSymbol.ourNil), LObject);
    }
    
    @Test
    public void testCons() {
        LObject cons = evaluateString("(cons (+ 4 5) \"hi\")");
        Assert.assertEquals(LispList.cons(new LispInteger(9), new LispString("hi")), cons);
    }

    @Test
    public void testToStringNils() {
        LObject cons = evaluateString("(cons (cons nil nil) nil)");
        Assert.assertEquals("((nil))", cons.toString());
    }

    @Test
    public void testMixedList() {
        LObject a = evaluateString("(cons 1 (cons 2 3))");
        Assert.assertEquals("(1 2 . 3)", a.toString());
        a = evaluateString("(cons 1 (cons 2 (cons 3 nil)))");
        Assert.assertEquals("(1 2 3)", a.toString());
        a = evaluateString("(cons 1 (cons (cons 5 6) 3))");
        Assert.assertEquals("(1 (5 . 6) . 3)", a.toString());
        a = evaluateString("(cons nil (cons nil nil))");
        Assert.assertEquals("(nil nil)", a.toString());
    }

    @Test
    public void testNReverse () {
        LObject reversed = evaluateString("(nreverse '(1 2))");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(1)), reversed);
        reversed = evaluateString("(nreverse '(1))");
        Assert.assertEquals(LispList.list(new LispInteger(1)), reversed);
    }

    @Test
    public void testNReverseCons () {
        try {
            evaluateString("(nreverse (cons 1 2))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp (1))", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testNReverseLong () {
        evaluateString("(setq x '(1 2 3))");
        LObject reversed = evaluateString("(nreverse x)");
        Assert.assertEquals(LispList.list(new LispInteger(1)), evaluateString("x"));
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(2), new LispInteger(1)), reversed);
    }
    
    @Test
    public void testNconcNil() {
        LObject r = evaluateString("(nconc)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testNconcOne() {
        LObject r = evaluateString("(nconc 1)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(nconc '(1))");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);        
    }

    @Test
    public void testNconcTwoSingle() {
        LObject r = evaluateString("(nconc '(1) 2)");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), r);
        evaluateString("(setq a '(1))");
        r = evaluateString("(nconc a '(2))");
        LispList expected = LispList.list(new LispInteger(1), new LispInteger(2));
        Assert.assertEquals(expected, r);
        Assert.assertEquals(expected, evaluateString("a"));
    }

    @Test
    public void testNconcTwoDouble() {
        LObject r = evaluateString("(nconc '(1 2) 3)");
        Assert.assertEquals(LispList.testList(new LispInteger(1), LispList.testList(new LispInteger(2), new LispInteger(3))), r);
        evaluateString("(setq a '(1 2))");
        r = evaluateString("(nconc a '(3))");
        LispList expected = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
        Assert.assertEquals(expected, r);
        Assert.assertEquals(expected, evaluateString("a"));
    }

    @Test
    public void testNconcAnyLastIsList() {
        evaluateString("(setq a '(1 2))");
        evaluateString("(setq b '(3 4))");
        evaluateString("(setq c '(5))");
        LObject r = evaluateString("(nconc a b c)");
        LispList expectedA = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3), new LispInteger(4), new LispInteger(5));
        Assert.assertEquals(expectedA, r);
        Assert.assertEquals(expectedA, evaluateString("a"));
        LispList expectedB = LispList.list(new LispInteger(3), new LispInteger(4), new LispInteger(5));
        Assert.assertEquals(expectedB, evaluateString("b"));
        Assert.assertEquals(LispList.list(new LispInteger(5)), evaluateString("c"));
    }

    @Test
    public void testNconcAnyLastIsOther() {
        evaluateString("(setq a '(1 2))");
        evaluateString("(setq b '(3 4))");
        evaluateString("(setq c \"q\")");
        LObject r = evaluateString("(nconc a b c)");
        LispList expectedA = LispList.testList(new LispInteger(1), new LispInteger(2), new LispInteger(3), LispList.testList(new LispInteger(4), new LispString("q")));
        Assert.assertEquals(expectedA, r);
        Assert.assertEquals(expectedA, evaluateString("a"));
        LispList expectedB = LispList.testList(new LispInteger(3), LispList.testList(new LispInteger(4), new LispString("q")));
        Assert.assertEquals(expectedB, evaluateString("b"));
        Assert.assertEquals(new LispString("q"), evaluateString("c"));
    }
    
    @Test
    public void testNthListElement() {
        evaluateString("(setq a '(1 2 3 4))");
        LObject r = evaluateString("(nth 1 a)");
        Assert.assertEquals(new LispInteger(2), r);
        r = evaluateString("(nth -1 a)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(nth 5 a)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }
    
    @Test
    public void testAssoc() {
        LObject r = evaluateString("(assoc 1 '(1 2 3 4 (1 . \"alla\") 5))");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispString("alla")), r);
        r = evaluateString("(assoc nil '(1 (nil . nil) 2))");
        Assert.assertEquals(LispList.list(), r);
        //Assert.assertEquals("(nil)", r.toString());
        r = evaluateString("(assoc 1 '((1 2 3) (nil . nil) 2))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("(assoc nil '((() 2 3) (nil . nil) 2))");
        Assert.assertEquals(LispList.list(LispSymbol.ourNil, new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("(assoc nil nil)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testDelq() {
        evaluateString("(setq a '(1 2 3))");
        LObject r = evaluateString("(delq 2 a)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(3)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(3)), r);
        r = evaluateString("(delq 3 a)");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);
        r = evaluateString("(delq 4 '(1 2 4 3 4))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
    }
    
    @Test
    public void testDelqFirst() {
        evaluateString("(setq a '(1 2 3))");
        LObject r = evaluateString("(setq b (delq 1 a))");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("(delq 3 b)");
        Assert.assertEquals(LispList.list(new LispInteger(2)), r);
        r = evaluateString("b");
        Assert.assertEquals(LispList.list(new LispInteger(2)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), r);
    }

    @Test
    public void testDelqTheOnly() {
        LObject r = evaluateString("(delq 1 '(1))");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testDelqMultiple() {
        LObject r = evaluateString("(delq 1 '(2 1 1 1))");
        Assert.assertEquals(LispList.list(new LispInteger(2)), r);
        evaluateString("(setq c '(1 1 1 1 1 1))");
        r = evaluateString("(delq 1 c)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("c");
        Assert.assertEquals("(1 1 1 1 1 1)", r.toString());
    }

    @Test
    public void testDelqWrong() {
        try {
            evaluateString("(delq 1 '(2 . 1))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp (2 . 1))", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
}
