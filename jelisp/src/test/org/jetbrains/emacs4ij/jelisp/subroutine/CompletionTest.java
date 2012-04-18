package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/17/12
 * Time: 8:51 AM
 * To change this template use File | Settings | File Templates.
 */
public class CompletionTest extends BaseSubroutineTest {
    @Test
    public void testTryCompletionStringList() {
        LispObject completion = evaluateString("(try-completion \"*\" '(\"*anna\" \"*alla\"))");
        Assert.assertEquals(new LispString("*a"), completion);
    }

    @Test
    public void testTryCompletionStringSymbolList() {
        LispObject completion = evaluateString("(try-completion \"*\" '(\"*anna\" *alla 1))");
        Assert.assertEquals(new LispString("*a"), completion);
    }

    @Test
    public void testTryCompletionStringSymbol() {
        LispObject completion = evaluateString("(try-completion \"*\" '(\"anna\" *alla 1))");
        Assert.assertEquals(new LispString("*alla"), completion);
    }

    @Test
    public void testTryCompletionNotLambda() {
        try {
            evaluateString("(try-completion \"*\" '(anna \"*alla\"))");
        } catch (Exception e) {
            //from 'funcall
            Assert.assertEquals("'(invalid-function (anna \"*alla\"))", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testTryCompletionAlistStringSymbol() {
        LispObject completion = evaluateString("(try-completion \"a\" '((anna \"*alla\")))");
        Assert.assertEquals(new LispString("anna"), completion);
    }

    @Test
    public void testTryCompletionListWithInt() {
        LispObject completion = evaluateString("(try-completion \"1\" '(\"*anna\" *alla 1))");
        Assert.assertEquals(LispSymbol.ourNil, completion);
    }

    @Test
    public void testTryCompletionNilArg() {
        LispObject completion = evaluateString("(try-completion \"nil\" '(\"a\" nil \"aalla\"))");
        Assert.assertEquals(LispSymbol.ourT, completion);
    }

    @Test
    public void testTryCompletionAssocList() {
        LispObject completion = evaluateString("(try-completion \"foo\" '((\"foo1\" 5 3) (\"foo2\" 2 3 2)))");
        Assert.assertEquals(new LispString("foo"), completion);
    }

    @Test
    public void testTryCompletionWithPredicate() {
        evaluateString("(defun test (s) (> (length (car s)) 6))");
        LispObject completion = evaluateString("(try-completion \"foo\" '((\"foobar1\" 1) (\"barfoo\" 2) (\"foobaz\" 3) (\"foobar2\" 4)) 'test)");
        Assert.assertEquals(new LispString("foobar"), completion);

        completion = evaluateString("(try-completion \"foo\" '((\"foobar1\" 1) (barfoo 2) (\"foobaz\" 3) (\"foobar2\" 4)) 'test)");
        Assert.assertEquals(new LispString("foobar"), completion);
    }

    @Test
    public void testTryCompletionWithPredicateFail() {
        try {
            evaluateString("(defun test (s) (> (length (car s)) 6))");
            evaluateString("(try-completion \"foo\" '((\"foobar1\" 1) (barfoo 2) (\"foobaz\" 3) (foobar2 4)) 'test)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument sequencep foobar2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testTryCompletionCaseFold() {
        evaluateString("(setq completion-ignore-case nil)");
        LispObject completion = evaluateString("(try-completion \"alla\" '(\"Alla\" \"allaz\"))");
        Assert.assertEquals(new LispString("allaz"), completion);
        evaluateString("(setq completion-ignore-case t)");
        completion = evaluateString("(try-completion \"alla\" '(\"Alla\" \"allaz\"))");
        Assert.assertEquals(new LispString("Alla"), completion);
    }

    @Test
    public void testTryCompletionCaseFoldT() {
        evaluateString("(setq completion-ignore-case nil)");
        LispObject completion = evaluateString("(try-completion \"Alla\" '(\"Alla\" \"alla\"))");
        Assert.assertEquals(LispSymbol.ourT, completion);
        evaluateString("(setq completion-ignore-case t)");
        completion = evaluateString("(try-completion \"Alla\" '(\"Alla\" \"alla\"))");
        Assert.assertEquals(LispSymbol.ourT, completion);
    }

    @Test
    public void testAllCompletions () {
        evaluateString("(defun test (s) (> (length (car s)) 6))");
        LispObject completion = evaluateString("(all-completions \"foo\" '((\"foobar2\" 1) (barfoo 2) (\"foobaz\" 3) (\"foobar1\" 4)) 'test)");
        Assert.assertEquals(LispList.list(new LispString("foobar2"), new LispString("foobar1")), completion);
    }

    @Test
    public void testAllCompletionsPredicateFail () {
        try {
            evaluateString("(defun test (s) (> (length (car s)) 6))");
            evaluateString("(all-completions \"foo\" '((\"foobar1\" 1) (barfoo 2) (\"foobaz\" 3) (foobar2 4)) 'test)");
        } catch (Exception e) {
            //from 'length
            Assert.assertEquals("'(wrong-type-argument sequencep foobar2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testTestCompletions() {
        evaluateString("(setq completion-ignore-case t)");
        LispObject test = evaluateString("(test-completion \"alla\" '(\"Alla\" \"allaz\"))");
        Assert.assertEquals(LispSymbol.ourT, test);
        evaluateString("(defun test1 (s) (equal (aref s 0) 65))");
        test = evaluateString("(test-completion \"alla\" '(\"allaz\" \"alla\" \"alla\") 'test1)");
        Assert.assertEquals(LispSymbol.ourNil, test);
    }
}
