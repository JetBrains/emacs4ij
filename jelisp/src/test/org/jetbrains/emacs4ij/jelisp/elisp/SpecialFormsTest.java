package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import org.junit.Assert;
import org.junit.Before;
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
public class SpecialFormsTest {
    private Environment environment;

    @Before
    public void setUp() throws Exception {
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
    public void testQuote() throws Exception {
        LObject LObject = evaluateString("'5");
        junit.framework.Assert.assertEquals(new LispInteger(5), LObject);
    }

    @Test
    public void testQuotedQuotedList () {
        LObject LObject = evaluateString("'(quote 5)");
        junit.framework.Assert.assertEquals(new LispList(new LispSymbol("quote"), new LispInteger(5)), LObject);
    }

    @Test
    public void testQuotedFunctionArg() {
        LObject kit = evaluateString("(defun kit (a) (car-safe a))");
        junit.framework.Assert.assertEquals("kit ", new LispSymbol("kit"), kit);
        LObject LObject = evaluateString("(kit 'test)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testLetEmpty() {
        LObject LObject = evaluateString("(let ())");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testLetEmptyVar() {
        LObject LObject = evaluateString("(let () 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), LObject);
    }

    @Test
    public void testLetNilVar() {
        LObject LObject = evaluateString("(let (a) a)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testLetEmptyBody() {
        LObject LObject = evaluateString("(let ((a 5)) )");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testLetAtomVar() {
        LObject LObject = evaluateString("(let ((a 5)) a)");
        junit.framework.Assert.assertEquals(new LispInteger(5), LObject);
    }

    @Test
    public void testLetStar() {
        LObject LObject = evaluateString("(let* ((a 5) (b (+ 2 a))) (+ a b))");
        junit.framework.Assert.assertEquals(new LispInteger(12), LObject);
    }

    @Test
    public void testCond() {
        LObject cond = evaluateString("(cond)");
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
            Throwable q = getCause(e);
            if (!(q instanceof WrongTypeArgumentException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testCondWrongArg2() throws Exception {
        try {
            evaluateString("(cond 5)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof WrongTypeArgumentException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testWhile() {
        evaluateString("(set 'my-list '(1 2 3))");
        LObject LObject = evaluateString("(while my-list (car my-list) (set 'my-list (cdr my-list)))");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, LObject);
    }

    @Test
    public void testIfT () {
        try {
            evaluateString("(if t)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof WrongNumberOfArgumentsException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testIfNil () {
        try {
            evaluateString("(if nil)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof WrongNumberOfArgumentsException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testIfTrue () {
        LObject LObject = evaluateString("(if 5 'true 'false)");
        junit.framework.Assert.assertEquals(new LispSymbol("true"), LObject);
    }

    @Test
    public void testIfFalse () {
        LObject LObject = evaluateString("(if () 'true 'one 'two 'false)");
        junit.framework.Assert.assertEquals(new LispSymbol("false"), LObject);
    }

    @Test
    public void testAndEmpty() {
        LObject and = evaluateString("(and)");
        junit.framework.Assert.assertEquals(LispSymbol.ourT, and);
    }

    @Test
    public void testAndNil() {
        LObject and = evaluateString("(and 1 2 3 nil)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, and);
    }

    @Test
    public void testAndVal() {
        LObject and = evaluateString("(and 1 2 3 4 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), and);
    }

    @Test
    public void testOrEmpty() {
        LObject or = evaluateString("(or)");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, or);
    }

    @Test
    public void testOrMulti() {
        LObject or = evaluateString("(or nil nil nil 5)");
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
        LObject varDoc = evaluateString("(get 'a 'variable-documentation)");
        junit.framework.Assert.assertEquals(new LispString("doc"), varDoc);
        LObject b = evaluateString("(set 'a 10)");
        junit.framework.Assert.assertEquals(new LispInteger(10), b);
        varDoc = evaluateString("(get 'a 'variable-documentation)");
        junit.framework.Assert.assertEquals(new LispString("doc"), varDoc);
    }

    @Test
    public void testDefun3args() {
        LObject fun = evaluateString("(defun mult7 (arg) (* 7 arg))");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        LObject value = evaluateString("(mult7 5)");
        junit.framework.Assert.assertEquals("mult7 return value assertion", new LispInteger(35), value);
    }

    @Test
    public void testDefun4args() {
        LObject fun = evaluateString("(defun mult7 (arg) \"multiplies arg*7\" (* 7 arg))");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        LObject value = evaluateString("(mult7 5)");
        junit.framework.Assert.assertEquals("mult7 return value assertion", new LispInteger(35), value);
    }

    @Test (expected = WrongNumberOfArgumentsException.class)
    public void testDefunWrongNumberOfArgs() {
        LObject fun = evaluateString("(defun mult7 () ())");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("mult7"), fun);
        evaluateString("(mult7 5)");
    }

    @Test
    public void testDefunEmptyBody() {
        LObject fun = evaluateString("(defun nilFun () ())");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("nilFun"), fun);
        LObject value = evaluateString("(nilFun)");
        junit.framework.Assert.assertEquals("nilFun return value assertion", LispSymbol.ourNil, value);
    }

    @Test
    public void testDefunEmptyBody2 () {
        LObject fun = evaluateString("(defun nilFun ())");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("nilFun"), fun);
        LObject value = evaluateString("(nilFun)");
        junit.framework.Assert.assertEquals("nilFun return value assertion", LispSymbol.ourNil, value);
    }

    @Test
    public void testDefunIntBody () {
        LObject fun = evaluateString("(defun testFun () 5)");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        LObject value = evaluateString("(testFun)");
        junit.framework.Assert.assertEquals("testFun return value assertion", new LispInteger(5), value);
    }

    @Test (expected = InvalidFunctionException.class)
    public void testDefunWrongBody () {
        LObject fun = evaluateString("(defun testFun () (5))");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        evaluateString("(testFun)");
    }

    @Test
    public void testDefunComplexBody () {
        LObject fun = evaluateString("(defun testFun () 5 6 7 8 'ann)");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        LObject value = evaluateString("(testFun)");
        junit.framework.Assert.assertEquals("testFun return value assertion", new LispSymbol("ann"), value);
    }

    @Test (expected = InvalidFunctionException.class)
    public void testDefunFalseBody () {
        LObject fun = evaluateString("(defun testFun 5)");
        junit.framework.Assert.assertEquals("defun return value assertion", new LispSymbol("testFun"), fun);
        evaluateString("(testFun)");
    }

    @Ignore
    @Test
    public void testDefunVoidBody () {
        //todo
    }

    @Test
    public void testFunctionSymbolArgumentsSubstitution() {
        evaluateString("(defun test (a) a)");
        LObject LObject = evaluateString("(test 5)");
        junit.framework.Assert.assertEquals(new LispInteger(5), LObject);
    }

    @Test
    public void testFunctionArgumentsSubstitution() {
        evaluateString("(defun test (a) (+ a (+ a 1)))");
        LObject LObject = evaluateString("(test 5)");
        junit.framework.Assert.assertEquals(new LispInteger(11), LObject);
    }

    @Test
    public void testDefunInsideLet() {
        evaluateString("(let ((x 2)) (defun one () 1))");
        LObject result = evaluateString("(one)");
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
        LObject result = evaluateString("one");
        junit.framework.Assert.assertEquals(new LispInteger(1), result);
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
        LObject lispObject = evaluateString("(progn 1 2 3)");
        Assert.assertEquals(new LispInteger(3), lispObject);
        evaluateString("(progn (defvar pvar 50))");
        lispObject = evaluateString("pvar");
        Assert.assertEquals(new LispInteger(50), lispObject);
    }

    @Test
    public void testFunctionDocumentationNil () {
        evaluateString("(defun a () \"doc\" 2)");
        LObject doc = evaluateString("(documentation-property 'a 'function-documentation)");
        Assert.assertEquals(LispSymbol.ourNil, doc);
    }

    @Test
    public void testDocumentationNoDef () {
        try {
            evaluateString("(documentation 'a)");
        } catch (Exception e) {
            Throwable q = getCause(e);
            if (!(q instanceof VoidFunctionException))
                Assert.fail(q.getLocalizedMessage());
        }
    }

    @Test
    public void testDocumentationString () {
        evaluateString("(defun a () \"doc\" 2)");
        LObject doc = evaluateString("(documentation 'a)");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testDocumentationProperty () {
        evaluateString("(defun a () 2)");
        evaluateString("(put 'a 'function-documentation \"doc\")");
        LObject doc = evaluateString("(documentation 'a)");
        Assert.assertEquals(new LispString("doc"), doc);
        doc = evaluateString("(documentation-property 'a 'function-documentation)");
        Assert.assertEquals(new LispString("doc"), doc);
    }

    @Test
    public void testSetqNil() {
        LObject lispObject = evaluateString("(setq)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSetqOverrideValue () {
        LObject lispObject = evaluateString("(defvar a 10 \"doc\")");
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
        LObject lispObject = evaluateString("(setq a 20)");
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
        LObject lispObject = evaluateString("(setq a 20)");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(20), lispObject);
        lispObject = evaluateString("(let () (setq a 15) a)");
        Assert.assertEquals(new LispInteger(15), lispObject);
        lispObject = evaluateString("a");
        Assert.assertEquals(new LispInteger(15), lispObject);
    }

    //todo: not string documentation

    @Ignore
    @Test
    public void testDefineMacro() throws Exception {
        throw new RuntimeException("not implemented");
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

}
