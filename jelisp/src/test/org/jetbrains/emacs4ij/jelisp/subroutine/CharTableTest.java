package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/21/12
 * Time: 1:34 PM
 * To change this template use File | Settings | File Templates.
 */
public class CharTableTest extends BaseSubroutineTest {
    @Test
    public void testMakeCharTable() throws Exception {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 10)");
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject r = evaluateString("(get (char-table-subtype ct) 'char-table-extra-slots)");
        Assert.assertEquals(new LispInteger(10), r);
        evaluateString("(put 'p 'char-table-extra-slots 6)");
        r = evaluateString("(get (char-table-subtype ct) 'char-table-extra-slots)");
        Assert.assertEquals(new LispInteger(6), r);
    }

    @Test
    public void testMakeCharTableNegativeExtra() throws Exception {
        try {
            evaluateString("(defvar p)");
            evaluateString("(put 'p 'char-table-extra-slots -1)");
            evaluateString("(defvar ct (make-char-table 'p))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument wholenump -1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMakeCharTableTooBigExtra() throws Exception {
        try {
            evaluateString("(defvar p)");
            evaluateString("(put 'p 'char-table-extra-slots 11)");
            evaluateString("(defvar ct (make-char-table 'p))");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range 11 nil)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCharTableP() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject t = evaluateString("(char-table-p ct)");
        Assert.assertEquals(LispSymbol.ourT, t);
        t = evaluateString("(char-table-p nil)");
        Assert.assertEquals(LispSymbol.ourNil, t);
    }

    @Test
    public void testGetExtraSlot() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p))");
        for (int s = 0; s < 10; ++s) {
            LispObject a = evaluateString("(char-table-extra-slot ct " + s + ')');
            Assert.assertEquals(LispSymbol.ourNil, a);
        }
    }

    @Test
    public void testGetExtraSlotLess() {
        try {
            evaluateString("(defvar p)");
            evaluateString("(put 'p 'char-table-extra-slots 5)");
            evaluateString("(defvar ct (make-char-table 'p))");
            evaluateString("(char-table-extra-slot ct -1)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range " + evaluateString("ct").toString() + " -1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testGetExtraSlotMore() {
        try {
            evaluateString("(defvar p)");
            evaluateString("(put 'p 'char-table-extra-slots 5)");
            evaluateString("(defvar ct (make-char-table 'p))");
            evaluateString("(char-table-extra-slot ct 10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range " + evaluateString("ct").toString() + " 10)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSetExtraSlot() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject a = evaluateString("(set-char-table-extra-slot ct 1 5)");
        Assert.assertEquals(new LispInteger(5), a);
        Assert.assertEquals(new LispInteger(5), evaluateString("(char-table-extra-slot ct 1)"));
    }

    @Test
    public void testGetParent() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject a = evaluateString("(char-table-parent ct)");
        Assert.assertEquals(LispSymbol.ourNil, a);
    }

    @Test
    public void testCharTableToString() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 0)");
        evaluateString("(defvar ct (make-char-table 'p 2))");
        LispObject a = evaluateString("ct");
        Assert.assertEquals("#^[2 nil p]", a.toString());

        evaluateString("(defvar ct1 (make-char-table 'q 2))");
        a = evaluateString("ct1");
        Assert.assertEquals("#^[2 nil q]",
                a.toString());

        evaluateString("(put 'p 'char-table-extra-slots 10)");
        evaluateString("(defvar ct2 (make-char-table 'p 2))");
        a = evaluateString("ct2");
        Assert.assertEquals("#^[2 nil p 2 2 2 2 2 2 2 2 2 2]",
                a.toString());
    }

    @Test
    public void testSetParent() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p 1))");
        evaluateString("(defvar ctp (make-char-table 'q 2))");
        LispObject a = evaluateString("(set-char-table-parent ct ctp)");
        Assert.assertEquals(evaluateString("ctp"), a);
        a = evaluateString("(set-char-table-parent ct nil)");
        Assert.assertEquals(LispSymbol.ourNil, a);
    }

    @Test
    public void testSetParentWrong() {
        try {
            evaluateString("(defvar ct (make-char-table 'p))");
            evaluateString("(set-char-table-parent ct 5)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument char-table-p 5)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSetRangeCons() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject a = evaluateString("(set-char-table-range ct '(1 . 3) 16)");
        Assert.assertEquals(new LispInteger(16), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p 16 16 16]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testSetRangeAsciiChar() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject a = evaluateString("(set-char-table-range ct ?a 16)");
        Assert.assertEquals(new LispInteger(16), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p 16]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testSetRangeT() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LispObject a = evaluateString("(set-char-table-range ct t 16)");
        Assert.assertEquals(new LispInteger(16), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testSetRangeTThenChar() {
        evaluateString("(defvar ct (make-char-table 'p))");
        evaluateString("(set-char-table-range ct t 16)");
        LispObject a = evaluateString("(set-char-table-range ct ?a 5)");
        Assert.assertEquals(new LispInteger(5), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p 5]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testRangeCons() {
        evaluateString("(defvar ct (make-char-table 'p))");
        evaluateString("(set-char-table-range ct ?a 5)");
        LispObject a = evaluateString("(char-table-range ct '(97 . 123))");
        Assert.assertEquals(new LispInteger(5), a);
    }

    @Test
    public void testRangeChar() {
        evaluateString("(defvar ct (make-char-table 'p))");
        evaluateString("(set-char-table-range ct t 16)");
        evaluateString("(set-char-table-range ct ?a 5)");
        LispObject a = evaluateString("(char-table-range ct ?a)");
        Assert.assertEquals(new LispInteger(5), a);
    }

    @Test
    public void testRangeWrong() {
        try {
            evaluateString("(defvar ct (make-char-table 'p))");
            evaluateString("(char-table-range ct t)");
        } catch (Exception e) {
            Assert.assertTrue(TestSetup.getCause(e).contains("Invalid RANGE argument to `char-table-range'"));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSyntaxTableIsCharTable() {
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(char-table-p (syntax-table))"));
    }

    @Test
    public void testMapCharTable() {
        evaluateString("(setq ct (make-char-table 'purpose 'a))");
        evaluateString("(set-char-table-range ct '(99 . 100) 'b)");
        LispObject ranges = evaluateString("(let (accumulator) (map-char-table\n" +
                "              #'(lambda (key value)\n" +
                "                  (setq accumulator\n" +
                "                        (cons (list\n" +
                "                               (if (consp key)\n" +
                "                                   (list (car key) (cdr key))\n" +
                "                                 key)\n" +
                "                               value)\n" +
                "                              accumulator)))\n" +
                "              ct)\n" +
                "             accumulator)");
        //(((101 4194303) a) ((99 100) b) ((0 98) a))
        Assert.assertEquals("(((101 4194303) a) ((99 100) b) ((0 98) a))", ranges.toString());
//        LispList expected = LispList.list(LispList.list(LispList.list(new LispInteger(101), new LispInteger())))

    }

}
