package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 3:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class SpecialFormsTest extends BaseSubroutineTest {
    @Test
    public void testQuote() throws Exception {
        LispObject LispObject = evaluateString("'5");
        junit.framework.Assert.assertEquals(new LispInteger(5), LispObject);
    }

    @Test
    public void testQuotedQuotedList () {
        LispObject LispObject = evaluateString("'(quote 5)");
        junit.framework.Assert.assertEquals(LispList.list(new LispSymbol("quote"), new LispInteger(5)), LispObject);
    }

    @Test
    public void testQuotedFunctionArg() {
        LispObject kit = evaluateString("(defun kit (a) (car-safe a))");
        junit.framework.Assert.assertEquals("kit ", new LispSymbol("kit"), kit);
        LispObject LispObject = evaluateString("(kit 'test)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testLetEmpty() {
        LispObject LispObject = evaluateString("(let ())");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testLetEmptyVar() {
        LispObject LispObject = evaluateString("(let () 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), LispObject);
    }

    @Test
    public void testLetNilVar() {
        LispObject LispObject = evaluateString("(let (a) a)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testLetEmptyBody() {
        LispObject LispObject = evaluateString("(let ((a 5)) )");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testLetAtomVar() {
        LispObject LispObject = evaluateString("(let ((a 5)) a)");
        junit.framework.Assert.assertEquals(new LispInteger(5), LispObject);
    }

    @Test
    public void testLetStar() {
        LispObject LispObject = evaluateString("(let* ((a 5) (b (+ 2 a))) (+ a b))");
        junit.framework.Assert.assertEquals(new LispInteger(12), LispObject);
    }

    @Test
    public void testCond() {
        LispObject cond = evaluateString("(cond)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, cond);
        cond = evaluateString("(cond (5))");
        junit.framework.Assert.assertEquals(new LispInteger(5), cond);
        cond = evaluateString("(cond (nil 10 15) (1 2 3))");
        junit.framework.Assert.assertEquals(new LispInteger(3), cond);
        cond = evaluateString("(cond (1 10 15) 5)");
        junit.framework.Assert.assertEquals(new LispInteger(15), cond);
    }

    @Test
    public void testCondWrongArg1() {
        try {
            evaluateString("(cond (nil 10 15) 5)");
        } catch (Exception e) {
            Throwable q = TestSetup.getCause(e);
            if (!(q instanceof WrongTypeArgumentException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testCondWrongArg2() throws Exception {
        try {
            evaluateString("(cond 5)");
        } catch (Exception e) {
            Throwable q = TestSetup.getCause(e);
            if (!(q instanceof WrongTypeArgumentException))
                Assert.fail(q.getLocalizedMessage());
        }
    }
    
    @Test
    public void testCondResult() {
        LispObject r = evaluateString("(cond (t (message \"a\") nil) (t (message \"b\")))");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testWhile() {
        evaluateString("(set 'my-list '(1 2 3))");
        LispObject LispObject = evaluateString("(while my-list (car my-list) (set 'my-list (cdr my-list)))");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testIfT () {
        try {
            evaluateString("(if t)");
        } catch (Exception e) {
            Throwable q = TestSetup.getCause(e);
            if (!(q instanceof WrongNumberOfArgumentsException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testIfNil () {
        try {
            evaluateString("(if nil)");
        } catch (Exception e) {
            Throwable q = TestSetup.getCause(e);
            if (!(q instanceof WrongNumberOfArgumentsException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testIfTrue () {
        LispObject LispObject = evaluateString("(if 5 'true 'false)");
        junit.framework.Assert.assertEquals(new LispSymbol("true"), LispObject);
    }

    @Test
    public void testIfFalse () {
        LispObject LispObject = evaluateString("(if () 'true 'one 'two 'false)");
        junit.framework.Assert.assertEquals(new LispSymbol("false"), LispObject);
    }

    @Test
    public void testAndEmpty() {
        LispObject and = evaluateString("(and)");
        junit.framework.Assert.assertEquals(LispSymbol.ourT, and);
    }

    @Test
    public void testAndNil() {
        LispObject and = evaluateString("(and 1 2 3 nil)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, and);
    }

    @Test
    public void testAndVal() {
        LispObject and = evaluateString("(and 1 2 3 4 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), and);
    }

    @Test
    public void testOrEmpty() {
        LispObject or = evaluateString("(or)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, or);
    }

    @Test
    public void testOrMulti() {
        LispObject or = evaluateString("(or nil nil nil 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), or);
    }

    @Test (expected = VoidVariableException.class)
    public void testDefvar1() {
        evaluateString("(defvar a)");
        evaluateString("a");
    }

    @Test
    public void testDefvar2() {
        evaluateString("(defvar a 5 \"doc\")");
        LispSymbol a = environment.find("a");
        junit.framework.Assert.assertEquals(new LispInteger(5), a.getValue());
        LispObject varDoc = evaluateString("(get 'a 'variable-documentation)");
        junit.framework.Assert.assertEquals(new LispString("doc"), varDoc);
        LispObject b = evaluateString("(set 'a 10)");
        junit.framework.Assert.assertEquals(new LispInteger(10), b);
        varDoc = evaluateString("(get 'a 'variable-documentation)");
        junit.framework.Assert.assertEquals(new LispString("doc"), varDoc);
    }

    @Test
    public void testDefun3args() {
        LispObject fun = evaluateString("(defun mult7 (arg) (* 7 arg))");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        LispObject value = evaluateString("(mult7 5)");
        junit.framework.Assert.assertEquals("mult7 return value assertion", new LispInteger(35), value);
    }

    @Test
    public void testDefun4args() {
        LispObject fun = evaluateString("(defun mult7 (arg) \"multiplies arg*7\" (* 7 arg))");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        LispObject value = evaluateString("(mult7 5)");
        junit.framework.Assert.assertEquals("mult7 return value assertion", new LispInteger(35), value);
    }

    @Test (expected = WrongNumberOfArgumentsException.class)
    public void testDefunWrongNumberOfArgs() {
        LispObject fun = evaluateString("(defun mult7 () ())");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        evaluateString("(mult7 5)");
    }

    @Test
    public void testDefunEmptyBody() {
        LispObject fun = evaluateString("(defun nilFun () ())");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("nilFun"), fun);
        LispObject value = evaluateString("(nilFun)");
        junit.framework.Assert.assertEquals("nilFun return value assertion", LispSymbol.ourNil, value);
    }

    @Test
    public void testDefunEmptyBody2 () {
        LispObject fun = evaluateString("(defun nilFun ())");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("nilFun"), fun);
        LispObject value = evaluateString("(nilFun)");
        junit.framework.Assert.assertEquals("nilFun return value assertion", LispSymbol.ourNil, value);
    }

    @Test
    public void testDefunIntBody () {
        LispObject fun = evaluateString("(defun testFun () 5)");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        LispObject value = evaluateString("(testFun)");
        junit.framework.Assert.assertEquals("testFun return value assertion", new LispInteger(5), value);
    }

    @Test (expected = InvalidFunctionException.class)
    public void testDefunWrongBody () {
        LispObject fun = evaluateString("(defun testFun () (5))");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        evaluateString("(testFun)");
    }

    @Test
    public void testDefunComplexBody () {
        LispObject fun = evaluateString("(defun testFun () 5 6 7 8 'ann)");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        LispObject value = evaluateString("(testFun)");
        junit.framework.Assert.assertEquals("testFun return value assertion", new LispSymbol("ann"), value);
    }

    @Test (expected = InvalidFunctionException.class)
    public void testDefunFalseBody () {
        LispObject fun = evaluateString("(defun testFun 5)");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        evaluateString("(testFun)");
    }
    
    @Test
    public void testDefVarNilDoc() {
        evaluateString("(defvar a 5 5)");
        LispObject doc = evaluateString("(documentation-property 'a 'variable-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
    }

    @Ignore
    @Test
    public void testDefunVoidBody () {
        //todo
    }

    @Test
    public void testFunctionSymbolArgumentsSubstitution() {
        evaluateString("(defun test (a) a)");
        LispObject LispObject = evaluateString("(test 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), LispObject);
    }

    @Test
    public void testFunctionArgumentsSubstitution() {
        evaluateString("(defun test (a) (+ a (+ a 1)))");
        LispObject LispObject = evaluateString("(test 5)");
        junit.framework.Assert.assertEquals(new LispInteger(11), LispObject);
    }

    @Test
    public void testDefunInsideLet() {
        evaluateString("(let ((x 2)) (defun one () 1))");
        LispObject result = evaluateString("(one)");
        junit.framework.Assert.assertEquals(new LispInteger(1), result);
    }

    @Test
    public void testDoubleDefvar() {
        evaluateString("(defvar a 1)");
        evaluateString("(defvar a 2 \"doc\")");
    }

    @Test
    public void testDefvarInsideLet() {
        evaluateString("(let ((x 2)) (defvar one 1))");
        LispObject result = evaluateString("one");
        junit.framework.Assert.assertEquals(new LispInteger(1), result);
    }
    
    @Test
    public void testDefconst() {
        LispObject a = evaluateString("(defconst a 5 5)");
        Assert.assertEquals(LispSymbol.ourNil, ((LispSymbol) a).getDocumentation());
        Assert.assertEquals(new LispInteger(5), evaluateString("a"));
        evaluateString("(setq a 10)");
        Assert.assertEquals(new LispInteger(10), evaluateString("a"));
    }

    @Test
    public void testSymbolWithValueAndFunctionCells() {
        evaluateString("(defvar a 1)");
        evaluateString("(defun a () 2)");
        junit.framework.Assert.assertEquals(new LispInteger(1), evaluateString("a"));
        junit.framework.Assert.assertEquals(new LispInteger(2), evaluateString("(a)"));
    }

    @Test
    public void testProgn() throws Exception {
        LispObject lispObject = evaluateString("(progn 1 2 3)");
        Assert.assertEquals(new LispInteger(3), lispObject);
        evaluateString("(progn (defvar pvar 50))");
        lispObject = evaluateString("pvar");
        Assert.assertEquals(new LispInteger(50), lispObject);

        evaluateString("(setq abc 123)");
        lispObject = evaluateString("abc");
        Assert.assertEquals(new LispInteger(123), lispObject);
        evaluateString("(progn (setq abc 50))");
        lispObject = evaluateString("abc");
        Assert.assertEquals(new LispInteger(50), lispObject);
    }

    @Test
    public void testSetqNil() {
        LispObject lispObject = evaluateString("(setq)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSetqOverrideValue () {
        LispObject lispObject = evaluateString("(defvar a 10 \"doc\")");
        Assert.assertEquals(new LispSymbol("a"), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(10), lispObject);
        lispObject = evaluateString("(setq a 20)");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("(get 'a 'variable-documentation)");
        Assert.assertEquals(new LispString("doc"), lispObject);
    }

    @Test
    public void testSetqMostLocalExistingBinding_Binding () {
        LispObject lispObject = evaluateString("(setq a 20)");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("(let ((a)) (setq a 15) a)");
        Assert.assertEquals(new LispInteger(15), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(20), lispObject);
    }

    @Test
    public void testSetqMostLocalExistingBinding_NoBinding () {
        LispObject lispObject = evaluateString("(setq a 20)");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("(let () (setq a 15) a)");
        Assert.assertEquals(new LispInteger(15), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(15), lispObject);
    }
    
    @Test
    public void testSetqListValue () {
        LispObject lispObject = evaluateString("(setq a '(b c))");
        Assert.assertEquals(LispList.list(new LispSymbol("b"), new LispSymbol("c")),  lispObject);
    }

    //todo: not string documentation


    @Test
    public void testDefunReturn () {
        LispObject funCell = evaluateString("(symbol-function (defun f ()))");
        Assert.assertTrue(funCell instanceof LispList);
        Assert.assertEquals(LispList.list(new LispSymbol("lambda"), LispList.list()), funCell);
    }

    @Test
    public void testInteractive_a() throws Exception {
        evaluateString("(defun g () (message \"plus = %d\" (+ 6 5)))");
        /*
        (defun f (fun) (interactive "aFunction: ") (funcall fun))
(defun f (buf) (interactive "bBuffer: ") (message "%s" (buffer-name (get-buffer buf))))
(defun f (buf) (interactive "BBuffer: ") (message "%s" (buffer-name (get-buffer buf))))
(defun f (ch)  (interactive "cCharacter: ") (message "%s" ch))
(commandp f)
(defun f (cmd) (interactive "CCommand: ") (funcall cmd))
(defun f (p) (interactive "dPoint: ") (message "%d" p))
default-directory
(progn (set-buffer "test1.lisp") default-directory)

(defun f (dir) (interactive "DDirectory: ") (message "%s" dir))
         */

    }

    @Test
    public void testDirectoryCompletion() {
        SpecialFormInteractive spi = new SpecialFormInteractive(environment, "D");
        List<String> completions = spi.getCompletions("~/Do");
        List<String> expected = new ArrayList<String>();
        String home = System.getProperty("user.home");
        expected.add(home + "/Documents");
        expected.add(home + "/Downloads");
        Assert.assertEquals(expected, completions);

        completions = spi.getCompletions("~");
        Assert.assertTrue(completions.isEmpty());

        completions = spi.getCompletions("~/");
        Assert.assertTrue(!completions.isEmpty());
    }


    @Test
    public void testDefineMacro_Simple() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 ())");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), LispList.list()), fCell);
    }

    @Test
    public void testDefineMacro_SimpleIgnoreArgType() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 5)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), new LispInteger(5)), fCell);
    }

    @Test
    public void testDefineMacro_WithDocString() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 () \"docstring\" nil)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), LispList.list(), new LispString("docstring"), LispSymbol.ourNil), fCell);
    }

    @Test (expected = WrongNumberOfArgumentsException.class)
    public void testDefineMacro_WrongNumberOfArguments() {
        evaluateString("(defmacro m1)");
    }

    @Test
    public void testDefineMacro_DeclareAfterArguments() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 () (declare (doc-string \"hello1\")) \"hello2\" nil)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), LispList.list(), new LispString("hello2"), LispSymbol.ourNil), fCell);
    }

    @Test
    public void testDefineMacro_DoubleDeclareAfterArguments() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 () (declare (doc-string \"hello1\")) (declare (doc-string \"hello2\")) \"hello3\" nil)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), LispList.list(), new LispString("hello3"), LispSymbol.ourNil), fCell);
    }

    @Test
    public void testDefineMacro_DeclareAfterDocstring() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 () \"hello2\" (declare (doc-string \"hello1\")) nil)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), LispList.list(), new LispString("hello2"), LispSymbol.ourNil), fCell);
    }

    @Test
    public void testDefineMacro_DoubleDeclareAfterDocstring() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 () \"hello3\" (declare (doc-string \"hello1\")) (declare (doc-string \"hello2\")) nil)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"), new LispSymbol("lambda"), LispList.list(), new LispString("hello3"), LispSymbol.ourNil), fCell);
    }

    @Test
    public void testDefineMacro_DeclareAfterDocstringAndAfterArguments() {
        LispSymbol macro = (LispSymbol) evaluateString("(defmacro m1 ()  (declare (doc-string \"hello1\")) \"hello2\" (declare (doc-string \"hello3\")) nil)");
        LispObject fCell = evaluateString("(symbol-function 'm1)");
        Assert.assertEquals(LispList.list(new LispSymbol("macro"),
                new LispSymbol("lambda"),
                LispList.list(),
                new LispString("hello2"),
                LispList.list(new LispSymbol("declare"), LispList.list(new LispSymbol("doc-string"), new LispString("hello3"))),
                LispSymbol.ourNil),
                fCell);
    }


    private static void conditionCaseErrorChecker (Object error, String expectedMessage) {
        if (error instanceof LispString) {
            // from signal
            Assert.assertEquals(expectedMessage, ((LispString) error).getData());
            return;
        }
        if (error instanceof RuntimeException) {
            Assert.assertEquals(expectedMessage, TestSetup.getCause((Throwable) error).getMessage());
            //return;
        }
    }

    // error message must be = (void-variable a)
    @Test
    public void testConditionCase_GlobalBinding () {
        LispObject result = evaluateString("(condition-case err (progn (+ a 5)) (void-variable \"message\"))");
        //LispObject result = evaluateString("(condition-case err (+ a 5) (void-variable \"message\"))");
        Assert.assertEquals(new LispString("message"), result);
        LispSymbol err = environment.find("err");
        Assert.assertNotNull(err);
        //todo: in err must be a LispList!
        //in this case err.value = (wrong-type-argument number-or-marker-p (wrong-type-argument number-or-marker-p (wrong-type-argument number-or-marker-p (void-variable a))))
        Assert.assertEquals(LispList.list(new LispSymbol("void-variable"), new LispSymbol("a")), err.getValue());
        
        //conditionCaseErrorChecker(err, "(void-variable a)");
    }

    @Test
    public void testConditionCase2 () {
        try {
            evaluateString("(condition-case a (+ a 5) 5 (void-variable \"message\"))");
        } catch (RuntimeException e) {
            conditionCaseErrorChecker(e, "Invalid condition handler");
        }
    }

    @Ignore
    @Test
    public void testConditionCase3 () {
        // in a must be stored errror-symbol
        LispObject r = evaluateString("(condition-case b (+ a 5) (wrong-type-argument (symbol-value 'b)))");
        // (wrong-type-argument number-or-marker-p (wrong-type-argument number-or-marker-p (void-variable a)))
        Assert.assertEquals(LispList.list(new LispSymbol("wrong-type-argument"),
                                        new LispSymbol("number-or-marker-p"),
                                        LispList.list(new LispSymbol("wrong-type-argument"),
                                                     new LispSymbol("number-or-marker-p"),
                                                LispList.list(new LispSymbol("void-variable"), new LispSymbol("a")))),
                r);
    }

    @Test
    public void testConditionCase_LocalBinding () {
        evaluateString("(setq b 123)");
        LispObject r = evaluateString("(condition-case b (+ a 5) (void-variable (symbol-value 'b)))");
        Assert.assertEquals(LispList.list(new LispSymbol("void-variable"), new LispSymbol("a")), r);
        Assert.assertEquals(new LispInteger(123), evaluateString("b"));
    }

    @Test
    public void testConditionCase_NilBinding () {
        LispObject r = evaluateString("(condition-case nil (+ a 5) (void-variable \"hi\"))");
        Assert.assertEquals(new LispString("hi"), r);
        r = evaluateString("(condition-case () (+ a 5) (void-variable \"hi\"))");
        Assert.assertEquals(new LispString("hi"), r);
    }

    @Test
    public void testDefunEnvironment() {
        evaluateString("(defun f (s) (cond ((eq s 4) (f 5) (message \"%d\" s))\n" +
                "\t\t   ((eq s 5) (message \"Second=%d\" s))))");
        LispObject r = evaluateString("(f 4)");
        Assert.assertEquals("\"4\"", r.toString());
    }


    @Test
    public void testNilChar() {
        LispObject r  = evaluateString("(setq b ?\\^( )");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testWrongChar() {
        try {
            evaluateString("(setq b ?^\\( )");
        } catch (Exception e) {
            Assert.assertEquals("'(invalid-read-syntax \"?\")", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testThrowCatch() {
        LispObject r = evaluateString("(catch 'a (message \"hi\") (+ 1 2) (throw 'a 5))");
        Assert.assertEquals(new LispInteger(5), r);
    }

    @Test
    public void testCatchNoThrow() {
        LispObject r = evaluateString("(catch 'a (message \"hi\") (+ 1 2))");
        Assert.assertEquals(new LispInteger(3), r);
    }

    @Test
    public void testCatchNoBody() {
        LispObject r = evaluateString("(catch 'a)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testCatchInnerThrow() {
        evaluateString("(defun f () (throw 'a 1))");
        LispObject r = evaluateString("(catch 'a (message \"hi\") (f) (+ 1 2))");
        Assert.assertEquals(new LispInteger(1), r);
    }
    
    @Test
    public void testFunction() {
        LispObject f = evaluateString("(defun f() (message \"hi\"))");
        LispObject r = evaluateString("(function f)");
        Assert.assertEquals(f, r);
        r = evaluateString("(function (lambda () (+ 2 3)))");
        Assert.assertEquals("(lambda nil (+ 2 3))", r.toString());
    } 

    @Test
    public void testInteractiveNil() {
        LispObject i = evaluateString("(interactive)");
        Assert.assertEquals(LispSymbol.ourNil, i);
        i = evaluateString("(interactive nil)");
        Assert.assertEquals(LispSymbol.ourNil, i);
        i = evaluateString("(interactive ())");
        Assert.assertEquals(LispSymbol.ourNil, i);
    }

    @Test
    public void testInteractiveList() {
        LispObject i = evaluateString("(interactive (list (+ 5 3)))");
        Assert.assertEquals(LispList.list(new LispInteger(8)), i);
    }

    @Test
    public void testInteractiveListWrong() {
        try {
            evaluateString("(interactive (+ 5 3))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 8)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testInteractiveInnerScope() {
        evaluateString("(defun f () (+ 3 6) (interactive))");
        LispObject r = evaluateString("(f)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }
}
