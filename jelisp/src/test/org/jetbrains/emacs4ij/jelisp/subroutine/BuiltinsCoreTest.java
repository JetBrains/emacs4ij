package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Before;
import org.junit.BeforeClass;
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

    @BeforeClass
    public static void runBeforeClass() {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null);
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
        Assert.assertEquals(LispList.list(new LispSymbol("setq"),
                                        new LispSymbol("r"),
                                        LispList.list(new LispSymbol("+"), new LispSymbol("r"), new LispInteger(1))),
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
        Assert.assertEquals(LispList.list(new LispSymbol("progn"),
                                         LispList.list(new LispSymbol("inc"), new LispSymbol("r")),
                                         LispList.list(new LispSymbol("inc"), new LispSymbol("s"))),
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
        Assert.assertEquals(LispList.list(new LispSymbol("f")), expansion);
    }
    
    @Test
    public void testFset() {
        LObject f = evaluateString("(fset 'f2 'f1)");
        Assert.assertEquals(new LispSymbol("f1"), f);
        LispSymbol f1 = environment.find("f1");
        Assert.assertNull(f1);
        LispSymbol f2 = environment.find("f2");
        Assert.assertNotNull(f2);
        Assert.assertEquals(new LispSymbol("f1"), f2.getFunction());
    }
    
    @Test
    public void testFsetWta() {
        try {
            evaluateString("(fset 5 10)");
        } catch (WrongTypeArgumentException e) {
            // todo:  (wrong-type-argument symbolp 5)
            Assert.assertEquals("'(wrong-type-argument LispSymbol 5)", e.getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testIndirectFunction() {
        evaluateString("(defun f1 () (+ 1 2))");
        evaluateString("(fset 'f2 'f1)");
        evaluateString("(fset 'g 'f2)");
        LObject innerF = evaluateString("(indirect-function 'g)");
        Assert.assertEquals("(lambda nil (+ 1 2))", innerF.toString());
    }

    @Test
    public void testIndirectFunctionNumber() {
        LObject r = evaluateString("(fset 'a 10)");
        Assert.assertEquals(new LispInteger(10), r);
        r = evaluateString("(indirect-function 'a)");
        Assert.assertEquals(new LispInteger(10), r);
    }

    @Test
    public void testIndirectFunctionVoid() {
        evaluateString("(fset 'g 'f)");
        try {
            evaluateString("(indirect-function 'g)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-function g)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testIndirectFunctionCycle() {
        evaluateString("(fset 'f 'g)");
        evaluateString("(fset 'g 'f)");
        try {
            evaluateString("(indirect-function 'g)");
        } catch (Exception e) {
            Assert.assertEquals("'(cyclic-function-indirection f)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testIndirectFunctionOverride() {
        evaluateString("(defun f () (+ 1 2))");
        LObject innerF = evaluateString("(indirect-function 'f)");
        LispSymbol f = environment.find("f");
        Assert.assertEquals(f.getFunction(), innerF);
        evaluateString("(fset 'f 1)");
        innerF = evaluateString("(indirect-function 'f)");
        Assert.assertEquals(new LispInteger(1), innerF);
    }

    @Test
    public void testPrimitiveMinMaxArgsNum() {
        LispSymbol f = environment.find("indirect-function");
        Assert.assertEquals(new LispInteger(1), ((Primitive) f.getFunction()).getMinNumArgs());
        Assert.assertEquals(new LispInteger(2), ((Primitive)f.getFunction()).getMaxNumArgs());

        f = environment.find("run-hooks");
        Assert.assertEquals(new LispInteger(0), ((Primitive) f.getFunction()).getMinNumArgs());
        Assert.assertEquals(new LispSymbol("many"), ((Primitive) f.getFunction()).getMaxNumArgs());

        f = environment.find("if");
        Assert.assertEquals(new LispInteger(2), ((Primitive) f.getFunction()).getMinNumArgs());
        Assert.assertEquals(new LispSymbol("unevalled"), ((Primitive)f.getFunction()).getMaxNumArgs());
    }
    
    @Test
    public void testSubrArityWta() {
        try {
            evaluateString("(subr-arity 'if)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument subrp if)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSubrArity() {
        LObject cons = evaluateString("(subr-arity (symbol-function 'if))");
        Assert.assertEquals(LispList.cons(new LispInteger(2), new LispSymbol("unevalled")), cons);
        cons = evaluateString("(subr-arity (symbol-function 'indirect-function))");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), cons);
        cons = evaluateString("(subr-arity (symbol-function 'run-hooks))");
        Assert.assertEquals(LispList.cons(new LispInteger(0), new LispSymbol("many")), cons);
    }

    @Test
    public void testArefVectorOk () {
        LObject val = evaluateString("(aref '[1 2 3] 0)");
        Assert.assertEquals(new LispInteger(1), val);
    }

    @Test
    public void testArefVectorOutOfBounds () {
        try {
            evaluateString("(aref '[1 2 3] 10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range [1 2 3] 10)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testArefStringOk () {
        LObject val = evaluateString("(aref \"hi\" 0)");
        Assert.assertEquals(new LispInteger(104), val);
    }

    @Test
    public void testArefStringOutOfBounds () {
        try {
            evaluateString("(aref \"hi\" 10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"hi\" 10)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

}
