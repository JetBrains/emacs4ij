package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Before;
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
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null, null);
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

    @Test
    public void testSetVar() throws LispException {
        LObject value = evaluateString("(set 'var (+ 2 3))");
        Assert.assertEquals("set return value assertion", new LispInteger(5), value);
        LObject lispObject = evaluateString("var");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testSetBindings() {
        LObject lispObject = evaluateString("(set 'one 1)");
        Assert.assertEquals(new LispInteger(1), lispObject);
        lispObject = evaluateString("(set 'two 'one)");
        Assert.assertEquals(new LispSymbol("one"), lispObject);
        lispObject = evaluateString("(set two 2)");
        Assert.assertEquals(new LispInteger(2), lispObject);
        lispObject = evaluateString("one");
        Assert.assertEquals(new LispInteger(2), lispObject);
        lispObject = evaluateString("(let ((one 1)) (set 'one 3) one)");
        Assert.assertEquals(new LispInteger(3), lispObject);
        lispObject = evaluateString("one");
        Assert.assertEquals(new LispInteger(2), lispObject);
    }

    @Test (expected = WrongTypeArgumentException.class)
    public void testSetSymbols() {
        LObject lispObject = evaluateString("(set 'x 1)");
        Assert.assertEquals(new LispInteger(1), lispObject);
        lispObject = evaluateString("(set 'y 'x)");
        Assert.assertEquals(new LispSymbol("x"), lispObject);
        lispObject = evaluateString("y");
        LispSymbol x = new LispSymbol("x", new LispInteger(1));
        Assert.assertEquals(x, lispObject);

        lispObject = evaluateString("(symbol-value y)");
        Assert.assertEquals(new LispInteger(1), lispObject);
        lispObject = evaluateString("(symbol-value 'y)");
        Assert.assertEquals(new LispSymbol("x"), lispObject);
        //must throw WrongTypeArgumentException
        evaluateString("(symbol-value x)");
    }

    @Test
    public void testEq() {
        LObject lispObject = evaluateString("(eq 5 5)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(eq 'foo 'foo)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(eq \"qwa\" \"qwa\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(eq \"\" \"\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(eq '(1 (2 (3))) '(1 (2 (3))))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        evaluateString("(setq foo '(1 (2 (3))))");
        lispObject = evaluateString("(eq foo foo)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(eq foo '(1 (2 (3))))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(eq [(1 2) 3] [(1 2) 3])");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);

        //todo: (eq (make-symbol "foo") 'foo) ⇒ nil
    }

    @Test
    public void testEqual() {
        LObject lispObject = evaluateString("(equal 5 5)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(equal 'foo 'foo)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(equal \"qwa\" \"qwa\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(equal \"qwa\" \"QWA\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(equal \"\" \"\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(equal '(1 (2 (3))) '(1 (2 (3))))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        evaluateString("(setq foo '(1 (2 (3))))");
        lispObject = evaluateString("(equal foo foo)");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(equal foo '(1 (2 (3))))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(equal [(1 2) 3] [(1 2) 3])");
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
    public void testFuncall () {
        LObject result = evaluateString("(funcall '+ 1 2)");
        Assert.assertEquals(new LispInteger(3), result);
    }

    @Test
    public void testFuncallInvalid () {
        try {
            evaluateString("(funcall 0 1 2)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            Assert.assertTrue(q instanceof InvalidFunctionException);
            return;
        }
        Assert.assertTrue(false);
    }

    @Test
    public void testRunHooks () {
        evaluateString("(defun f () (+ 1 2))");
        evaluateString("(defun g () (+ 2 3))");
        evaluateString("(set 'hook1 '(f g))");
        evaluateString("(run-hooks 'hook1)");
        evaluateString("(run-hooks 'hook2)");
    }

    @Test
    public void testRunHooks_InvalidFunction () {
        try {
            evaluateString("(set 'hook1 5)");
            evaluateString("(run-hooks 'hook1)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            Assert.assertTrue(q instanceof InvalidFunctionException);
            return;
        }
        Assert.assertTrue(false);
    }

    @Test
    public void testMacro () {
        evaluateString("(setq r 5)");
        evaluateString("(defmacro inc (var) (list 'setq var (list '+ var 1)))");
        evaluateString("(inc r)");
        LObject r = evaluateString("r");
        Assert.assertEquals(new LispInteger(6), r);
    }


    @Test
    public void testMacroExpand_Complex () {
        evaluateString("(defmacro inc (var) (list 'setq var (list '+ var 1)))");
        LObject expansion = evaluateString("(macroexpand '(inc r))");
        //(setq r (+ r 1))
        Assert.assertEquals(new LispList(new LispSymbol("setq"),
                                        new LispSymbol("r"),
                                        new LispList(new LispSymbol("+"), new LispSymbol("r"), new LispInteger(1))),
                expansion);
    }

    @Test
    public void testMacroExpand_Simple () {
        evaluateString("(defmacro m2 (q) (declare (doc-string \"hello1\")) \"hello2\" (+ 5 q))");
        LObject expansion = evaluateString("(macroexpand '(m2 7))");
        Assert.assertEquals(new LispInteger(12), expansion);
    }

    @Test
    public void testMacroExpand_EmbeddedMacro () {
        evaluateString("(defmacro inc (var) (list 'setq var (list '+ var 1)))");
        evaluateString("(defmacro inc2 (var1 var2) (list 'progn (list 'inc var1) (list 'inc var2)))");
        LObject expansion = evaluateString("(macroexpand '(inc2 r s))");
        //(progn (inc r) (inc s))  ; inc not expanded here.
        Assert.assertEquals(new LispList(new LispSymbol("progn"),
                                         new LispList(new LispSymbol("inc"), new LispSymbol("r")),
                                         new LispList(new LispSymbol("inc"), new LispSymbol("s"))),
                expansion);
    }

    @Test
    public void testMacroExpand_NotMacroCall() {
        LObject expansion = evaluateString("(macroexpand 10)");
        Assert.assertEquals(new LispInteger(10), expansion);
        evaluateString("(defun f ())");
        expansion = evaluateString("(macroexpand 'f)");
        Assert.assertEquals(new LispSymbol("f"), expansion);
        expansion = evaluateString("(macroexpand '(f))");
        Assert.assertEquals(new LispList(new LispSymbol("f")), expansion);
    }

}
