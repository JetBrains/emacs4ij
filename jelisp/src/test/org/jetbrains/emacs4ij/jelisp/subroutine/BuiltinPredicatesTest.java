package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/18/11
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinPredicatesTest {
    private CustomEnvironment environment;

    @BeforeClass
    public static void runBeforeClass() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs 23.2a/emacs-23.2");
        GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.2");
        GlobalEnvironment.initialize(null, null);
        GlobalEnvironment.INSTANCE.startRecording();
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
        lispObject = evaluateString("(functionp (symbol-function 'subrp))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(functionp (symbol-function 'if))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(functionp  'subrp)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Ignore
    @Test
    public void FunctionpLambda() {
        LObject lispObject = evaluateString("(functionp (lambda () 1))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
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

    @Test
    public void testDefaultBoundP() {
        LObject result = evaluateString("(default-boundp 'f)");
        Assert.assertEquals(LispSymbol.ourNil, result);
        evaluateString("(setq f 1)");
        result = evaluateString("(default-boundp 'f)");
        Assert.assertEquals(LispSymbol.ourT, result);
        result = evaluateString("(default-boundp 'default-directory)");
        Assert.assertEquals(LispSymbol.ourT, result);
        result = evaluateString("(default-boundp 'is-alive)");
        Assert.assertEquals(LispSymbol.ourNil, result);
    }

    @Test
    public void testSequenceP() {
        LObject r = evaluateString("(sequencep ())");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep '())");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep nil)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep \"hello\")");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep [])");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep '[])");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep '[1 2])");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(sequencep '(1 2))");
        Assert.assertEquals(LispSymbol.ourT, r);

        r = evaluateString("(sequencep 'a)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }
}
