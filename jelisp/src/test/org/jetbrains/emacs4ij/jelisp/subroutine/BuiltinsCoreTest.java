package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsCoreTest extends BaseSubroutineTest {
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
    }

    @Test
    public void testEqSymbol() {
        LObject lispObject = evaluateString("(eq (make-symbol \"foo\") 'foo)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
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
    public void testNilEqual() {
        LObject r = evaluateString("(equal nil (cons nil nil))");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(equal nil '())");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(equal nil '(nil))");
        Assert.assertEquals(LispSymbol.ourNil, r);
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
    public void testEvalLambda() {
        LObject result = evaluateString("((lambda (a) (+ 1 a)) 2)");
        Assert.assertEquals(new LispInteger(3), result);
    }

    @Test
    public void testFuncallLambda () {
        LObject result = evaluateString("(funcall '(lambda (a) (+ 1 a)) 2)");
        Assert.assertEquals(new LispInteger(3), result);
    }

    @Test
    public void testFuncallInvalid () {
        try {
            evaluateString("(funcall 0 1 2)");
        } catch (Exception e) {
            Throwable q = TestSetup.getCause(e);
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
            Throwable q = TestSetup.getCause(e);
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
            Assert.assertEquals("'(void-function g)", TestSetup.getCause(e).getMessage());
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
            Assert.assertEquals("'(cyclic-function-indirection f)", TestSetup.getCause(e).getMessage());
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
            Assert.assertEquals("'(wrong-type-argument subrp if)", TestSetup.getCause(e).getMessage());
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
            Assert.assertEquals("'(args-out-of-range [1 2 3] 10)", TestSetup.getCause(e).getMessage());
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
            Assert.assertEquals("'(args-out-of-range \"hi\" 10)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testAsetArefString() {
        evaluateString("(defvar s \"hello\")");
        LObject a = evaluateString("(aset s 1 ?\\C-z)");
        Assert.assertEquals(new LispInteger(26), a);
        a = evaluateString("s");
        Assert.assertEquals(evaluateString("s"), a);
        System.out.println(evaluateString("s").toString());
        a = evaluateString("(aref s 1)");
        Assert.assertEquals(new LispInteger(26), a);
        a = evaluateString("(length s)");
        Assert.assertEquals(new LispInteger(5), a);
    }

    @Test(expected = WrongNumberOfArgumentsException.class)
    public void testApplyNoArgs() {
        evaluateString("(apply '+)");
    }

    @Test
    public void testApplyNotListLastArg() {
        try {
            evaluateString("(apply '+ 1 2)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 2)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testApplyBuiltin() {
        LObject r = evaluateString("(apply '+ 1 2 '(3 4))");
        Assert.assertEquals(new LispInteger(10), r);
    }

    @Test
    public void testApplyBuiltinWrongArg() {
        try {
            evaluateString("(apply '< 1 2)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 2)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testApplySpecForm() {
        try {
            evaluateString("(apply 'setq 'a '(5))");
        } catch (Exception e) {
            Assert.assertEquals("'(invalid-function setq)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testApplyMacro() {
        try {
            evaluateString("(apply 'when 't '(message \"hi\"))");
        } catch (Exception e) {
            Assert.assertEquals("'(invalid-function when)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testApply() {
        LObject b = evaluateString("(nconc (nreverse '((ch (aref --cl-vec-- --cl-idx--)))))");
        Assert.assertEquals("((ch (aref --cl-vec-- --cl-idx--)))", b.toString());
        LObject r = evaluateString("(apply 'nconc '((ch (aref --cl-vec-- --cl-idx--))))");
        Assert.assertEquals("(ch (aref --cl-vec-- --cl-idx--))", r.toString());
        r = evaluateString("(apply 'nconc (nreverse '((ch (#<subr aref> --cl-vec-- --cl-idx--)))))");
        Assert.assertEquals("(ch (#<subr aref> --cl-vec-- --cl-idx--))", r.toString());
    }

    @Test
    public void testDefAliasVoidVar() {
        try {
            evaluateString("(defalias a nil)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-variable a)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testDefAliasVoidFun() {
        evaluateString("(defun f ())");
        LObject r = evaluateString("(defalias 'a (symbol-function 'f))");
        Assert.assertEquals("(lambda nil)", r.toString());
    }

    @Test
    public void testDefAliasSetDoc() {
        evaluateString("(defun f ())");
        LObject r = evaluateString("(defalias 'a (symbol-function 'f) (make-marker))");
        Assert.assertEquals("(lambda nil)", r.toString());
        r = evaluateString("(symbol-function 'a)");
        Lambda test = new Lambda(LispList.list(new LispSymbol("lambda"), LispSymbol.ourNil), environment);
        test.setDocumentation(new LispMarker());
        Assert.assertEquals(test, r);
        r = evaluateString("(get 'variable-documentation 'a)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(documentation 'a)");
        Assert.assertEquals(new LispMarker(), r);
        r = evaluateString("(documentation 'f)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }
    
    @Test
    public void testDefAliasExec() {
        evaluateString("(defalias 'a 'identity)");
        LObject r = evaluateString("(a 5)");
        Assert.assertEquals(new LispInteger(5), r);
        r = evaluateString("(symbol-function 'a)");
        Assert.assertEquals(new LispSymbol("identity"), r);
    }

    @Test
    public void testAtom() {
        LObject r = evaluateString("(atom 5)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(atom [])");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(atom nil)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(atom 'a)");
        Assert.assertEquals(LispSymbol.ourT, r);
        r = evaluateString("(atom ())");
        Assert.assertEquals(LispSymbol.ourT, r);

        r = evaluateString("(atom '(5))");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(atom (cons 1 2))");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testBackQuoteSimplest_Atom() {
        String form = "5";
        LObject expected = evaluateString("'" + form);
        LObject r = evaluateString("`" + form);
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteSimplest_Symbol() {
        String form = "a";
        LObject expected = evaluateString("'" + form);
        LObject r = evaluateString("`" + form);
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteSimplest_Vector() {
        String form = "[]";
        LObject expected = evaluateString("'" + form);
        LObject r = evaluateString("`" + form);
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteSimplest_List() {
        String form = "(1 a)";
        LObject expected = evaluateString("'" + form);
        LObject r = evaluateString("`" + form);
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteRecursiveList() {
        String form = "(1 a (5))";
        LObject expected = evaluateString("'" + form);
        LObject r = evaluateString("`" + form);
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteSimple() {
        LispList expected = LispList.list(new LispSymbol("a"), new LispSymbol("list"),
                new LispSymbol("of"), LispList.list(new LispSymbol("+"), new LispInteger(2), new LispInteger(3)),
                new LispSymbol("elements"));
        LObject r = evaluateString("'(a list of (+ 2 3) elements)");
        Assert.assertEquals(expected, r);
        r = evaluateString("`(a list of (+ 2 3) elements)");
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteComma() {
        LObject expected = evaluateString("(list 'a 'list 'of (+ 2 3) 'elements)");  //(a list of 5 elements)
        LObject result = evaluateString("`(a list of ,(+ 2 3) elements)");
        Assert.assertEquals(expected, result);
    }

    @Test
    public void testBackQuoteCommaDeep() {
        evaluateString("(defmacro t-becomes-nil (variable)\n" +
                "       `(if (eq ,variable t)\n" +
                "            (setq ,variable nil)))");
        evaluateString("(setq foo t)");
        LObject r = evaluateString("(t-becomes-nil foo)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("foo");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testBackQuoteDog_Simple() {
        evaluateString("(setq some-list '(2 3))");
        LObject expected = evaluateString("some-list");
        Assert.assertEquals("(2 3)", expected.toString());
        LObject r = evaluateString("some-list");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("`,@some-list");
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteDog_SimpleList() {
        evaluateString("(setq some-list '(2 3))");
        LObject expected = evaluateString("some-list");
        Assert.assertEquals("(2 3)", expected.toString());
        LObject r = evaluateString("some-list");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("`(,@some-list)");
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteDog_List() {
        evaluateString("(setq some-list '(2 3))");
        LObject expected = evaluateString("(cons 1 some-list)");
        Assert.assertEquals("(1 2 3)", expected.toString());
        LObject r = evaluateString("some-list");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("`(1 ,@some-list)");
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteListify() {
        LObject r = evaluateString("(backquote-listify '((2 . (8 9))) '(0 . nil))");
        Assert.assertEquals(LispList.list(new LispSymbol("list"), LispList.list(new LispInteger(8), new LispInteger(9))), r);
    }

    @Test
    public void testBackQuoteDog() {
        evaluateString("(setq some-list '(2 3))");
        LObject expected = evaluateString("(cons 1 (append some-list '(4) some-list))");
        Assert.assertEquals("(1 2 3 4 2 3)", expected.toString());
        LObject r = evaluateString("some-list");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("`(1 ,@some-list 4 ,@some-list)");
        Assert.assertEquals(expected, r);
    }

    @Test
    public void testBackQuoteAtoms () {
        evaluateString("(setq a 1 b 2)");
        LObject r = evaluateString("`(,a b)");
        Assert.assertEquals("(1 b)", r.toString());
    }

    @Test
    public void testBackQuoteBrackets () {
        evaluateString("(setq some-list '(2 3))");
        LObject r = evaluateString("`((,@some-list))");
        Assert.assertEquals("((2 3))", r.toString());
    }
    
    @Test (expected = LispThrow.class)
    public void testThrowNoCatch() {
        evaluateString("(throw 'a nil)");
    }

    @Test
    public void testListStarTwo() {
        evaluateString("(setq a 1)");
        evaluateString("(setq b '(2 3))");
        LObject r = evaluateString("(list* a b)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
    }

    @Test
    public void testListStarThreeNil() {
        evaluateString("(setq a 1)");
        evaluateString("(setq b '(2 3))");
        LObject r = evaluateString("(list* a b nil)");
        Assert.assertEquals(LispList.list(new LispInteger(1), LispList.list(new LispInteger(2), new LispInteger(3))), r);
    }

    @Test
    public void testListStarThreeObject() {
        evaluateString("(setq a 1)");
        evaluateString("(setq b '(2 3))");
        LObject r = evaluateString("(list* a b 4)");
        Assert.assertEquals(LispList.testList(new LispInteger(1),
                LispList.list(new LispInteger(2), new LispInteger(3)), new LispInteger(4)), r);
    }

    @Test
    public void testListStarThreeList() {
        evaluateString("(setq a 1)");
        evaluateString("(setq b '(2 3))");
        LObject r = evaluateString("(list* a b '(4))");
        Assert.assertEquals(LispList.list(new LispInteger(1),
                LispList.list(new LispInteger(2), new LispInteger(3)), new LispInteger(4)), r);
    }

    @Test
    public void testDefineMinorMode() {
        LObject r = evaluateString("(define-minor-mode m1 \"doc\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testSimple() {
        LObject r = GlobalEnvironment.INSTANCE.find("defface");
        Assert.assertNotNull(r);
        r = GlobalEnvironment.INSTANCE.find("defgroup");
        Assert.assertNotNull(r);

        GlobalEnvironment.INSTANCE.addSkipFunctions("eval-when-compile", "declare-function");
        GlobalEnvironment.INSTANCE.loadFile("simple.el");
    }
}
