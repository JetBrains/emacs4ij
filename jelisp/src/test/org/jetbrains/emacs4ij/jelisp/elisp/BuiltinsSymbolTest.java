package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsSymbolTest {

    private Environment environment;

    @Before
    public void setUp() {
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null);
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
    public void testSymbolFunction () {
        LObject lispObject = evaluateString("(symbol-function '+)");
        Assert.assertEquals(new LispString("#<subr +>"), new LispString(lispObject.toString()));
    }

    @Test (expected = VoidVariableException.class)
    public void testSymbolFunctionVoidVar() {
        evaluateString("(symbol-function a)");
    }

    @Test
    public void testSymbolFunctionVoidFunNonExistentSymbol() {
        try {
            evaluateString("(symbol-function 'a)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof VoidFunctionException))
                org.junit.Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testSymbolFunctionVoidFunExistingSymbol() {
        try {
            evaluateString("(defvar a 10)");
            evaluateString("(symbol-function 'a)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof VoidFunctionException))
                org.junit.Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test (expected = WrongTypeArgument.class)
    public void testSymbolFunctionWrongArg() {
        evaluateString("(symbol-function 5)");
    }

    @Test
    public void testGet() throws Exception {
        evaluateString("(set 'a 5)");
        LObject LObject = evaluateString("(get 'a 'p2)");
        Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testPut() throws Exception {
        evaluateString("(set 'a 5)");
        evaluateString("(put 'a 'p1 'v1)");
        LObject lispObject = evaluateString("(get 'a 'p1)");
        Assert.assertEquals(new LispSymbol("v1"), lispObject);
    }

    @Test
    public void testSymbolValueInteger() {
        evaluateString("(set 'a 5)");
        LObject lispObject = evaluateString("(symbol-value 'a)");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testSymbolValueVoid() {
        try {
            evaluateString("(symbol-value 'a)");
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof VoidVariableException);
            return;
        }
        Assert.assertTrue(false);
    }
}
