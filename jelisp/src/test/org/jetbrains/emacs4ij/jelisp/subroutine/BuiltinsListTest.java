package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
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
    private Environment environment;

    @BeforeClass
    public static void runBeforeClass() {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null,  null);
        GlobalEnvironment.getInstance().startRecording();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.getInstance().clearRecorded();
        environment = new Environment(GlobalEnvironment.getInstance());
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
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
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
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
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
            Assert.assertEquals("(wrong-type-argument listp 3)", getCause(e).getMessage());
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
            Assert.assertEquals("'(wrong-type-argument listp (1))", getCause(e).getMessage());
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


}
