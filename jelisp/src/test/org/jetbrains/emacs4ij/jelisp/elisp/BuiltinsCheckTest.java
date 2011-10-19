package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/18/11
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsCheckTest {
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
    public void testStringp() throws Exception {
        LObject lispObject = evaluateString("(stringp \"hello\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(stringp 'hello)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSymbolp() throws Exception {
        LObject lispObject = evaluateString("(symbolp \"hello\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(symbolp 'hello)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testIntegerp() throws Exception {
        LObject lispObject = evaluateString("(integerp 1)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(integerp 'hello)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSubrp() throws Exception {
        LObject lispObject = evaluateString("(subrp 1)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(subrp (symbol-function 'if))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(subrp 'if)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(subrp (symbol-function 'put))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testFunctionp () {
        evaluateString("(defun f () )");
        LObject lispObject = evaluateString("(functionp 'f)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(functionp (symbol-function 'f))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
      //  lispObject = evaluateString("(functionp (lambda () 1))");
      //  Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(functionp (symbol-function 'subrp))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(functionp (symbol-function 'if))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(functionp  'subrp)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }



}
