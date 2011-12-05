package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Before;
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

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null, null);
        environment = new Environment(GlobalEnvironment.getInstance());
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
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

    @Test (expected = WrongTypeArgumentException.class)
    public void testCarWrongArg() {
        evaluateString("(set 'p 'defun)");
        evaluateString("(car p)");
    }

    @Test
    public void testCdr() {
        evaluateString("(set 'p '(1 2 3))");
        LObject LObject = evaluateString("(cdr p)");
        Assert.assertEquals(new LispList(new LispInteger(2), new LispInteger(3)), LObject);
        evaluateString("(set 'p '(1))");
        LObject = evaluateString("(cdr p)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test (expected = WrongTypeArgumentException.class)
    public void testCdrWrongArg() {
        evaluateString("(set 'p 'defun)");
        evaluateString("(cdr p)");
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
        Assert.assertEquals(new LispList(new LispInteger(3)), LObject);
    }

    @Test
    public void testList() {
        LObject LObject = evaluateString("(list)");
        Assert.assertEquals("no args", LispSymbol.ourNil, LObject);
        LObject = evaluateString("(list 5 \"test\")");
        Assert.assertEquals("2 args", new LispList(new LispInteger(5), new LispString("test")), LObject);
        LObject = evaluateString("(list nil)");
        Assert.assertEquals("list of nil -1", new LispList(LispSymbol.ourNil), LObject);
        LObject = evaluateString("(list (list))");
        Assert.assertEquals("list of nil -2", new LispList(LispSymbol.ourNil), LObject);
    }
}
