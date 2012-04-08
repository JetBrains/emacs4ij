package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/18/11
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinPredicatesTest extends BaseSubroutineTest {
    @Test
    public void testStringp() throws Exception {
        LispObject lispObject = evaluateString("(stringp \"hello\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(stringp 'hello)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSymbolp() throws Exception {
        LispObject lispObject = evaluateString("(symbolp \"hello\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(symbolp 'hello)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testIntegerp() throws Exception {
        LispObject lispObject = evaluateString("(integerp 1)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(integerp 'hello)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSubrp() throws Exception {
        LispObject lispObject = evaluateString("(subrp 1)");
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
        LispObject lispObject = evaluateString("(functionp 'f)");
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

    @Test
    public void FunctionpLambda() {
        LispObject lispObject = evaluateString("(functionp (lambda () 1))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testCommandp () {
        evaluateString("(defun f () (interactive) )");
        LispObject lispObject = evaluateString("(commandp 'f)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(commandp 1)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testCommandpLambda () {
        LispObject lispObject = evaluateString("(commandp '(lambda () (+ 6 3) (interactive \"f\")))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testCommandp_BuiltIn () {
        LispObject lispObject = evaluateString("(commandp 'switch-to-buffer)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(commandp 'if)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testCommandpKeymap () {
        LispObject lispObject = evaluateString("(commandp 'Control-X-prefix)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testFboundp () {
        evaluateString("(defun f ())");
        LispObject result = evaluateString("(fboundp 'f)");
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
        LispObject result = evaluateString("(default-boundp 'f)");
        Assert.assertEquals(LispSymbol.ourNil, result);
        evaluateString("(setq f 1)");
        result = evaluateString("(default-boundp 'f)");
        Assert.assertEquals(LispSymbol.ourT, result);
    }

    @Test
    public void testDefaultBoundPDefaultDir() {
        LispObject result = evaluateString("(default-boundp 'default-directory)");
        Assert.assertEquals(LispSymbol.ourT, result);
        result = evaluateString("(default-boundp 'is-alive)");
        Assert.assertEquals(LispSymbol.ourNil, result);
    }

    @Test
    public void testSequenceP() {
        LispObject r = evaluateString("(sequencep ())");
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
    
    @Test
    public void testCharacterP() {
        LispObject r = evaluateString("(characterp 5)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(characterp -1)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(characterp 4194304)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(characterp 4194303)");
        Assert.assertEquals(LispSymbol.ourT, r);
    }
}
