package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/26/11
 * Time: 4:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class BListTest extends BaseSubroutineTest {

    @Test
    public void testInnerLists() throws LispException {
        LispObject LispObject = evaluateString("(+ 2 (+ 2 3))");
        Assert.assertEquals(new LispInteger(7), LispObject);
    }

    @Test
    public void testCar() {
        evaluateString("(set 'p '(1 2 3))");
        LispObject LispObject = evaluateString("(car p)");
        Assert.assertEquals(new LispInteger(1), LispObject);
        evaluateString("(set 'p '())");
        LispObject = evaluateString("(car p)");
        Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testCarNil() {
        LispObject res = evaluateString("(car nil)");
        Assert.assertEquals(LispSymbol.ourNil, res);
    }

    @Test
    public void testCarWrongArg() {
        try {
            evaluateString("(set 'p 'defun)");
            evaluateString("(car p)");
        } catch (Exception e) {
            Assert.assertTrue(TestSetup.getCause(e).contains("wrong-type-argument"));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCdr() {
        evaluateString("(set 'p '(1 2 3))");
        LispObject LispObject = evaluateString("(cdr p)");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), LispObject);
        evaluateString("(set 'p '(1))");
        LispObject = evaluateString("(cdr p)");
        Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testCdrWrongArg() {
        try {
            evaluateString("(set 'p 'defun)");
            evaluateString("(cdr p)");
        } catch (Exception e) {
            Assert.assertTrue(TestSetup.getCause(e).contains("wrong-type-argument"));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCdrNil() {
        LispObject res = evaluateString("(cdr nil)");
        Assert.assertEquals(LispSymbol.ourNil, res);
    }

    @Test
    public void testCarSafe() {
        evaluateString("(set 'p 'defun)");
        LispObject LispObject = evaluateString("(car-safe p)");
        Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testCdrSafe() throws Exception {
        evaluateString("(set 'p 'defun)");
        LispObject LispObject = evaluateString("(cdr-safe p)");
        Assert.assertEquals(LispSymbol.ourNil, LispObject);
    }

    @Test
    public void testMemq () {
        evaluateString("(set 'a '(1 2 3))");
        LispObject LispObject = evaluateString("(memq 4 a)");
        Assert.assertEquals("not exist", LispSymbol.ourNil, LispObject);
        LispObject = evaluateString("(memq 3 a)");
        Assert.assertEquals(LispList.list(new LispInteger(3)), LispObject);
    }

    @Test
    public void testMemqCons() {
        try {
            evaluateString("(setq x (cons 1 (cons (cons 5 6) 3)))");
            evaluateString("(memq 6 x)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 3)", TestSetup.getCause(e));
        }
    }

    @Test
    public void testList() {
        LispObject r = evaluateString("(list)");
        Assert.assertEquals("no args", LispSymbol.ourNil, r);
        r = evaluateString("(list 5 \"test\")");
        Assert.assertEquals("2 args", LispList.list(new LispInteger(5), new LispString("test")), r);
        r = evaluateString("(list nil)");
        Assert.assertEquals("list of nil -1", LispList.list(LispSymbol.ourNil), r);
        r = evaluateString("(list (list))");
        Assert.assertEquals("list of nil -2", LispList.list(LispSymbol.ourNil), r);
        r = evaluateString("(list 1 nil)");
        Assert.assertEquals(LispList.list(new LispInteger(1), LispSymbol.ourNil), r);
    }

    @Test
    public void testListN() {
        evaluateString("(setq a '(1 2))");
        evaluateString("(setq b '(3 4))");
        LispObject list = evaluateString("(list a b)");
        Assert.assertEquals(LispList.list(LispList.list(new LispInteger(1), new LispInteger(2)), LispList.list(new LispInteger(3), new LispInteger(4))), list);

        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), evaluateString("a"));
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(4)), evaluateString("b"));
    }

    @Test
    public void testCons() {
        LispObject cons = evaluateString("(cons (+ 4 5) \"hi\")");
        Assert.assertEquals(LispList.cons(new LispInteger(9), new LispString("hi")), cons);
    }

    @Test
    public void testToString() {
        evaluateString("(setq a '(and (< (setq --cl-idx-- (1+ --cl-idx--)) (length --cl-vec--)) (progn (setq ch (aref --cl-vec-- --cl-idx--)) (setq --cl-flag-- (and (characterp ch) (let ((ch2 (logand ch (lognot 134217728)))) (and (>= ch2 0) (<= ch2 127))))))))");
        evaluateString("(setq b (list 'a '()))");
        LispObject r = evaluateString("(list '(1 2) b)");
        System.out.println(r.toString());
    }

    @Test
    public void testToStringNilsSimple() {
        LispObject cons = evaluateString("(cons nil nil)");
        Assert.assertEquals("(nil)", cons.toString());
    }

    @Test
    public void testToStringNils() {
        LispObject cons = evaluateString("(cons (cons nil nil) nil)");
        Assert.assertEquals("((nil))", cons.toString());
    }

    @Test
    public void testToStringCons() {
        LispObject a = evaluateString("(cons 1 2)");
        Assert.assertEquals("(1 . 2)", a.toString());
    }

    @Test
    public void testToStringComplex() {
        LispObject a = evaluateString("'(5 (2 . 1) (5 6) \"hi\")");
        Assert.assertEquals("(5 (2 . 1) (5 6) \"hi\")", a.toString());
    }


    @Test
    public void testMixedList() {
        LispObject a = evaluateString("(cons 1 (cons 2 3))");
        Assert.assertEquals("(1 2 . 3)", a.toString());
        a = evaluateString("(cons 1 (cons 2 (cons 3 nil)))");
        Assert.assertEquals("(1 2 3)", a.toString());
        a = evaluateString("(cons 1 (cons (cons 5 6) 3))");
        Assert.assertEquals("(1 (5 . 6) . 3)", a.toString());
        a = evaluateString("(cons nil (cons nil nil))");
        Assert.assertEquals("(nil nil)", a.toString());
    }

    @Test
    public void testNReverse () {
        LispObject reversed = evaluateString("(nreverse '(1 2))");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(1)), reversed);
        reversed = evaluateString("(nreverse '(1))");
        Assert.assertEquals(LispList.list(new LispInteger(1)), reversed);
    }

    @Test
    public void testNReverseCons () {
        try {
            evaluateString("(nreverse (cons 1 2))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp (1))", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testNReverseLong () {
        evaluateString("(setq x '(1 2 3))");
        LispObject reversed = evaluateString("(nreverse x)");
        Assert.assertEquals(LispList.list(new LispInteger(1)), evaluateString("x"));
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(2), new LispInteger(1)), reversed);
    }

    @Test
    public void testNconcNil() {
        LispObject r = evaluateString("(nconc)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testNconcOne() {
        LispObject r = evaluateString("(nconc 1)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(nconc '(1))");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);
        r = evaluateString("(nconc nil '(1))");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);
    }

    @Test
    public void testNconcTwoSingle() {
        LispObject r = evaluateString("(nconc '(1) 2)");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), r);
        evaluateString("(setq a '(1))");
        r = evaluateString("(nconc a '(2))");
        LispList expected = LispList.list(new LispInteger(1), new LispInteger(2));
        Assert.assertEquals(expected, r);
        Assert.assertEquals(expected, evaluateString("a"));
    }

    @Test
    public void testNconcTwoDouble() {
        LispObject r = evaluateString("(nconc '(1 2) 3)");
        Assert.assertEquals(LispList.testList(new LispInteger(1), LispList.testList(new LispInteger(2), new LispInteger(3))), r);
        evaluateString("(setq a '(1 2))");
        r = evaluateString("(nconc a '(3))");
        LispList expected = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
        Assert.assertEquals(expected, r);
        Assert.assertEquals(expected, evaluateString("a"));
    }

    @Test
    public void testNconcAnyLastIsList() {
        evaluateString("(setq a '(1 2))");
        evaluateString("(setq b '(3 4))");
        evaluateString("(setq c '(5))");
        LispObject r = evaluateString("(nconc a b c)");
        LispList expectedA = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3), new LispInteger(4), new LispInteger(5));
        Assert.assertEquals(expectedA, r);
        Assert.assertEquals(expectedA, evaluateString("a"));
        LispList expectedB = LispList.list(new LispInteger(3), new LispInteger(4), new LispInteger(5));
        Assert.assertEquals(expectedB, evaluateString("b"));
        Assert.assertEquals(LispList.list(new LispInteger(5)), evaluateString("c"));
    }

    @Test
    public void testNconcAnyLastIsOther() {
        evaluateString("(setq a '(1 2))");
        evaluateString("(setq b '(3 4))");
        evaluateString("(setq c \"q\")");
        LispObject r = evaluateString("(nconc a b c)");
        LispList expectedA = LispList.testList(new LispInteger(1), new LispInteger(2), new LispInteger(3), LispList.testList(new LispInteger(4), new LispString("q")));
        Assert.assertEquals(expectedA, r);
        Assert.assertEquals(expectedA, evaluateString("a"));
        LispList expectedB = LispList.testList(new LispInteger(3), LispList.testList(new LispInteger(4), new LispString("q")));
        Assert.assertEquals(expectedB, evaluateString("b"));
        Assert.assertEquals(new LispString("q"), evaluateString("c"));
    }

    @Test
    public void testNconcWithNils() {
        LispObject r = evaluateString("(nconc '(1 2) nil '(3) nil)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
    }

    @Test
    public void testNthListElement() {
        evaluateString("(setq a '(1 2 3 4))");
        LispObject r = evaluateString("(nth 1 a)");
        Assert.assertEquals(new LispInteger(2), r);
        r = evaluateString("(nth -1 a)");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(nth 5 a)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testAssoc() {
        LispObject r = evaluateString("(assoc 1 '(1 2 3 4 (1 . \"alla\") 5))");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispString("alla")), r);
        r = evaluateString("(assoc nil '(1 (nil . nil) 2))");
        Assert.assertEquals(LispList.cons(LispSymbol.ourNil, LispSymbol.ourNil), r);
        r = evaluateString("(assoc 1 '((1 2 3) (nil . nil) 2))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("(assoc nil '((() 2 3) (nil . nil) 2))");
        Assert.assertEquals(LispList.list(LispSymbol.ourNil, new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("(assoc nil nil)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testDelq() {
        evaluateString("(setq a '(1 2 3))");
        LispObject r = evaluateString("(delq 2 a)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(3)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(3)), r);
        r = evaluateString("(delq 3 a)");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1)), r);
        r = evaluateString("(delq 4 '(1 2 4 3 4))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
    }

    @Test
    public void testDelqFirst() {
        evaluateString("(setq a '(1 2 3))");
        LispObject r = evaluateString("(setq b (delq 1 a))");
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
        r = evaluateString("(delq 3 b)");
        Assert.assertEquals(LispList.list(new LispInteger(2)), r);
        r = evaluateString("b");
        Assert.assertEquals(LispList.list(new LispInteger(2)), r);
        r = evaluateString("a");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2)), r);
    }

    @Test
    public void testDelqTheOnly() {
        LispObject r = evaluateString("(delq 1 '(1))");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testDelqMultiple() {
        LispObject r = evaluateString("(delq 1 '(2 1 1 1))");
        Assert.assertEquals(LispList.list(new LispInteger(2)), r);
        evaluateString("(setq c '(1 1 1 1 1 1))");
        r = evaluateString("(delq 1 c)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("c");
        Assert.assertEquals("(1 1 1 1 1 1)", r.toString());
    }

    @Test
    public void testDelqWrong() {
        try {
            evaluateString("(delq 1 '(2 . 1))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp (2 . 1))", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testNthCdrList() {
        LispObject r = evaluateString("(nthcdr 2 '(1 2 3))");
        Assert.assertEquals (LispList.list(new LispInteger(3)), r);
    }

    @Test
    public void testNthCdrCons() {
        LispObject r = evaluateString("(nthcdr 1 '(1 . 2))");
        Assert.assertEquals (new LispInteger(2), r);
    }

    @Test
    public void testNthCdrConsWrong() {
        try {
            evaluateString("(nthcdr 2 '(1 . 2))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testNthCdrZero() {
        LispObject r = evaluateString("(nthcdr 0 '(1 . 2))");
        Assert.assertEquals (LispList.cons(new LispInteger(1), new LispInteger(2)), r);
        r = evaluateString("(nthcdr 0 '(1 2 3))");
        Assert.assertEquals (LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
    }

    @Test
    public void testNthCdrNil() {
        LispObject r = evaluateString("(nthcdr 5 nil)");
        Assert.assertEquals (LispSymbol.ourNil, r);
        r = evaluateString("(nthcdr 4 '(1 2 3))");
        Assert.assertEquals (LispSymbol.ourNil, r);
        r = evaluateString("(nthcdr -1 '(1 2 3))");
        Assert.assertEquals (LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
    }

    @Test
    public void testAssq() {
        evaluateString("(setq foo '(1 (2 (3))))");
        LispObject r = evaluateString("(assq '(1 (2 (3))) '(5 nil \"str\" ((1 (2 (3))) 6) ((1 (2 (3))) . 3)))");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(assq 5 '(1 \"hi\" (5 . 3) (5 1)))");
        Assert.assertEquals (LispList.cons(new LispInteger(5), new LispInteger(3)), r);
    }

    @Test
    public void testSetcdr() {
        evaluateString("(defvar b (list 1 2 nil))");
        LispObject r = evaluateString("(setcdr b 3)");
        Assert.assertEquals(new LispInteger(3), r);
        r = evaluateString("b");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(3)), r);
    }

    @Test
    public void testSetcdrNil() {
        evaluateString("(defvar b (list 1 nil))");
        LispObject b = evaluateString("b");
        Assert.assertEquals(LispList.list(new LispInteger(1), LispSymbol.ourNil), b);
        evaluateString("(setcdr b 2)");
        b = evaluateString("b");
        Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), b);
        evaluateString("(setcdr b nil)");
        b = evaluateString("b");
        Assert.assertEquals("(1)", b.toString());
        Assert.assertEquals(LispList.list(new LispInteger(1)), b);
    }

    @Test
    public void testAppend () {
        evaluateString("(setq l2 (list 1 2 3))");
        LispObject list = evaluateString("(append l2 4)");
        Assert.assertEquals(LispList.testList(new LispInteger(1), new LispInteger(2), new LispInteger(3), new LispInteger(4)), list);
        Assert.assertEquals("(1 2 3 . 4)", list.toString());

        list = evaluateString("(append l2 '(4))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3), new LispInteger(4)), list);
        Assert.assertEquals("(1 2 3 4)", list.toString());

        evaluateString("(setq l1 (list 1 (cons 2 3)))");
        list = evaluateString("(append l1 4)");
        Assert.assertEquals("(1 (2 . 3) . 4)", list.toString());
        list = evaluateString("(append l1 '(4))");
        Assert.assertEquals("(1 (2 . 3) 4)", list.toString());
    }

    @Test
    public void testAfterAppend() {
        evaluateString("(setq k '(keymap))");
        evaluateString("(setq k (append k '(keymap)))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(consp (cdr k))"));
    }

    @Test
    public void testLengthAppended() {
        evaluateString("(setq l1 (list 1 2 3))");
        evaluateString("(setq a (append l1 4))");
        try {
            evaluateString("(length a)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 4)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSortSimple() {
        evaluateString("(setq a '(9 5 6 4 1))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(4), new LispInteger(5),
                new LispInteger(6), new LispInteger(9)), evaluateString("(sort a '<)"));
        Assert.assertEquals(LispList.list(new LispInteger(9)), evaluateString("a"));
    }

    @Test
    public void testSortGenericPredicate() {
        evaluateString("(setq a '(9 5 1 4 6))");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(4), new LispInteger(5),
                new LispInteger(6), new LispInteger(9)), evaluateString("(sort a '(lambda (v u) (< v u)))"));
        Assert.assertEquals(LispList.list(new LispInteger(9)), evaluateString("a"));
    }
}