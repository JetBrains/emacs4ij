package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/1/12
 * Time: 10:12 AM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsCharTableTest extends BaseSubroutineTest {
    @Test
    public void testMakeCharTable() throws Exception {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 10)");
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject r = evaluateString("(get (char-table-subtype ct) 'char-table-extra-slots)");
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
            Assert.assertEquals("'(wrong-type-argument wholenump -1)", TestSetup.getCause(e).getMessage());
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
            Assert.assertEquals("'(args-out-of-range 11 nil)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testCharTableP() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject t = evaluateString("(char-table-p ct)");
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
            LObject a = evaluateString("(char-table-extra-slot ct " + s + ')');
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
            Assert.assertEquals("'(args-out-of-range " + evaluateString("ct").toString() + " -1)", TestSetup.getCause(e).getMessage());
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
            Assert.assertEquals("'(args-out-of-range " + evaluateString("ct").toString() + " 10)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSetExtraSlot() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject a = evaluateString("(set-char-table-extra-slot ct 1 5)");
        Assert.assertEquals(new LispInteger(5), a);
        Assert.assertEquals(new LispInteger(5), evaluateString("(char-table-extra-slot ct 1)"));
    }

    @Test
    public void testGetParent() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject a = evaluateString("(char-table-parent ct)");
        Assert.assertEquals(LispSymbol.ourNil, a);
    }

    @Test
    public void testCharTableToString() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 0)");
        evaluateString("(defvar ct (make-char-table 'p 2))");
        LObject a = evaluateString("ct");
        Assert.assertEquals("#^[2 nil p 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]",
                a.toString());

        evaluateString("(defvar ct1 (make-char-table 'q 2))");
        a = evaluateString("ct1");
        Assert.assertEquals("#^[2 nil q 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]",
                a.toString());

        evaluateString("(put 'p 'char-table-extra-slots 10)");
        evaluateString("(defvar ct2 (make-char-table 'p 2))");
        a = evaluateString("ct2");
        Assert.assertEquals("#^[2 nil p 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]",
                a.toString());
    }

    @Test
    public void testSetParent() {
        evaluateString("(defvar p)");
        evaluateString("(put 'p 'char-table-extra-slots 5)");
        evaluateString("(defvar ct (make-char-table 'p 1))");
        evaluateString("(defvar ctp (make-char-table 'q 2))");
        LObject a = evaluateString("(set-char-table-parent ct ctp)");
        Assert.assertEquals(evaluateString("ctp"), a);
//        System.out.println(evaluateString("ct").toString());
        a = evaluateString("(set-char-table-parent ct nil)");
        Assert.assertEquals(LispSymbol.ourNil, a);
    }

    @Test
    public void testSetParentWrong() {
        try {
            evaluateString("(defvar ct (make-char-table 'p))");
            evaluateString("(set-char-table-parent ct 5)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument char-table-p 5)", TestSetup.getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testSetRangeCons() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject a = evaluateString("(set-char-table-range  ct '(1 . 3) 16)");
        Assert.assertEquals(new LispInteger(16), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p "
        +"#^^[3 0 nil 16 16 16 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 "
        +"#^^[3 0 nil 16 16 16 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testSetRangeAsciiChar() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject a = evaluateString("(set-char-table-range  ct ?a 16)");
        Assert.assertEquals(new LispInteger(16), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p "
        +"#^^[3 0 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 16 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] #^^[1 0 #^^[2 0 "
        +"#^^[3 0 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 16 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testSetRangeT() {
        evaluateString("(defvar ct (make-char-table 'p))");
        LObject a = evaluateString("(set-char-table-range ct t 16)");
        Assert.assertEquals(new LispInteger(16), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testSetRangeTThenChar() {
        evaluateString("(defvar ct (make-char-table 'p))");
        evaluateString("(set-char-table-range ct t 16)");
        LObject a = evaluateString("(set-char-table-range ct ?a 5)");
        Assert.assertEquals(new LispInteger(5), a);
        a = evaluateString("ct");
        String expected = "#^[nil nil p "
                + "#^^[3 0 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 5 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16] #^^[1 0 #^^[2 0 "
                + "#^^[3 0 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 5 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16] 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16] 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16] 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16 16]";
        Assert.assertEquals(expected, a.toString());
    }

    @Test
    public void testRangeCons() {
        evaluateString("(defvar ct (make-char-table 'p))");
        evaluateString("(set-char-table-range ct ?a 5)");
        LObject a = evaluateString("(char-table-range ct '(97 . 123))");
        Assert.assertEquals(new LispInteger(5), a);
    }

    @Test
    public void testRangeChar() {
        evaluateString("(defvar ct (make-char-table 'p))");
        evaluateString("(set-char-table-range ct t 16)");
        evaluateString("(set-char-table-range ct ?a 5)");
        LObject a = evaluateString("(char-table-range ct ?a)");
        Assert.assertEquals(new LispInteger(5), a);
    }

    @Test
    public void testRangeWrong() {
        try {
            evaluateString("(defvar ct (make-char-table 'p))");
            evaluateString("(char-table-range ct t)");
        } catch (Exception e) {
            Assert.assertTrue(TestSetup.getCause(e).getMessage().contains("Invalid RANGE argument to `char-table-range'"));
            return;
        }
        Assert.fail();
    }

}
