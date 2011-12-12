package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
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

    @BeforeClass
    public static void runBeforeClass() {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null, null);
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
      //  todo: lispObject = evaluateString("(functionp (lambda () 1))");
      //  todo: Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(functionp (symbol-function 'subrp))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(functionp (symbol-function 'if))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(functionp  'subrp)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testCommandp () {
        evaluateString("(defun f () (interactive) )");
        LObject lispObject = evaluateString("(commandp 'f)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(commandp 1)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testCommandp_BuiltIn () {
        LObject lispObject = evaluateString("(commandp 'switch-to-buffer)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(commandp 'if)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testFboundp () {
        evaluateString("(defun f ())");
        LObject result = evaluateString("(fboundp 'f)");
        Assert.assertEquals(LispSymbol.ourT, result);
        result = evaluateString("(fboundp 'if)");
        Assert.assertEquals(LispSymbol.ourT, result);
        result = evaluateString("(fboundp 'fboundp)");
        Assert.assertEquals(LispSymbol.ourT, result);
        result = evaluateString("(fboundp 'switch-to-buffer)");
        Assert.assertEquals(LispSymbol.ourT, result);
    }



}
