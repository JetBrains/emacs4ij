package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsCoreTest {
    private Environment environment;

    @Before
    public void setUp() throws Exception {
        Environment.ourEmacsPath = "/usr/share/emacs/23.2";
        Environment global = new Environment();
        environment = new Environment(global);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
    }

    @Test
    public void testPlusInteger() throws LispException {
        LObject lispObject = evaluateString("(+ 2 2)");
        Assert.assertEquals(new LispInteger(4), lispObject);
    }

    @Test
    public void testPlusFloat () {
        LObject lispObject = evaluateString("(+ 2 2.0)");
        Assert.assertEquals(new LispFloat(4), lispObject);
    }

    @Test
    public void testPlusSimple () {
        LispNumber n = BuiltinsCore.plus(new LispInteger(5), new LispFloat(6.6));
        Assert.assertEquals(new LispFloat(11.6), n);
    }

    @Test
    public void testMultiplySimple () {
        LispNumber n = BuiltinsCore.multiply(new LispInteger(5), new LispFloat(2.0));
        Assert.assertEquals(new LispFloat(10), n);
    }

    @Test
    public void testPlusEmpty () {
        LObject lispObject = evaluateString("(+)");
        Assert.assertEquals(new LispInteger(0), lispObject);

    }

    @Test
    public void testMultiply() throws Exception {
        LObject LObject = evaluateString("(* 2 2)");
        Assert.assertEquals(new LispInteger(4), LObject);
    }

    @Ignore
    @Test
    public void testSetVar() throws LispException {
        LObject value = evaluateString("(set 'var (+ 2 3))");
        Assert.assertEquals("set return value assertion", new LispInteger(5), value);
        LObject lispObject = evaluateString("var");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testEq() {
        LObject lispObject = evaluateString("(eq 5 5)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testNull () {
        LObject lispObject = evaluateString("(null 5)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(null nil)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testLispNot() throws Exception {
        LObject lispObject = evaluateString("(not 5)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(not nil)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testTest() {
        LObject lispObject = evaluateString("(test 1 2 3)");
        Assert.assertNull(lispObject);
    }
}
