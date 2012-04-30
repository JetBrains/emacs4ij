package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NoMatchData;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/27/12
 * Time: 11:06 AM
 * To change this template use File | Settings | File Templates.
 */
public class MatchTest extends BaseSubroutineTest {

    @Test (expected = NoMatchData.class)
    public void testMatchBeginningNil() {
        evaluateString("(match-beginning 0)");
    }

    @Test
    public void testMatchBeginningOutOfRange() {
        try {
            evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
            evaluateString("(match-beginning -10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range -10 0)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMatchBeginningZero() {
        evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
        LispObject r = evaluateString("(match-beginning 0)");
        Assert.assertEquals(new LispInteger(4), r);
        r = evaluateString("(match-beginning 1)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testMatchBeginningTwo() {
        evaluateString("(string-match \"one\" \"Test string one.\")");
        evaluateString("(string-match \"\\(qu\\)\\(ick\\)\" \"The quick fox jumped quickly.\")");
        LispObject r = evaluateString("(match-beginning 0)");
        Assert.assertEquals(new LispInteger(4), r);
        r = evaluateString("(match-beginning 1)");
        Assert.assertEquals(new LispInteger(4), r);
        r = evaluateString("(match-beginning 2)");
        Assert.assertEquals(new LispInteger(6), r);
        r = evaluateString("(match-beginning 3)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testMatchEnd() {
        evaluateString("(string-match \"one\" \"Test string one.\")");
        evaluateString("(string-match \"\\(qu\\)\\(ick\\)\" \"The quick fox jumped quickly.\")");
        LispObject r = evaluateString("(match-end 0)");
        Assert.assertEquals(new LispInteger(9), r);
        r = evaluateString("(match-end 1)");
        Assert.assertEquals(new LispInteger(6), r);
        r = evaluateString("(match-end 2)");
        Assert.assertEquals(new LispInteger(9), r);
        r = evaluateString("(match-end 3)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testMatchStringOutOfRange() {
        try {
            evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
            evaluateString("(match-string -1)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range -1 0)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMatchString() {
        LispObject r = evaluateString("(string-match \"\\(qu\\)\\(ick\\)\" \"The quick fox jumped quickly.\")");
        Assert.assertEquals(new LispInteger(4), r);
        r = evaluateString("(match-string 0 \"The quick fox jumped quickly.\")");
        Assert.assertEquals(new LispString("quick"), r);
        r = evaluateString("(match-string 1 \"The quick fox jumped quickly.\")");
        Assert.assertEquals(new LispString("qu"), r);
        r = evaluateString("(match-string 2 \"The quick fox jumped quickly.\")");
        Assert.assertEquals(new LispString("ick"), r);
        r = evaluateString("(match-string 3 \"The quick fox jumped quickly.\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testMatchDataFromStringSearch() {
        evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
        LispObject data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9)), data);
    }

    @Test
    public void testMatchDataFromStringSearchReuseListNoMarkers() {
        evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
        evaluateString("(setq reuse '(1 . 2))");
        LispObject data = evaluateString("(match-data nil reuse)");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9)), data);
        data = evaluateString("reuse");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9)), data);
    }

    @Test
    public void testMatchDataFromStringSearchReuseNotListNoMarkers() {
        evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
        evaluateString("(setq reuse 5)");
        LispObject data = evaluateString("(match-data nil reuse)");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9)), data);
        data = evaluateString("reuse");
        Assert.assertEquals(new LispInteger(5), data);
    }

    @Test
    public void testMatchDataWithBufferResult() {
        LispObject match = evaluateString("(string-match \"\\\\(qu\\\\)\\\\(ick\\\\)\" \"The quick fox jumped quickly.\")");
        Assert.assertEquals(new LispInteger(4), match);
        LispObject res = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), new LispInteger(4), new LispInteger(6),
                new LispInteger(6), new LispInteger(9)), res);

        //todo: how to get this result ?
        //(#<marker at 4 in *scratch*>
        // #<marker at 9 in *scratch*>
        // #<marker at 4 in *scratch*>
        // #<marker at 6 in *scratch*>
        // #<marker at 6 in *scratch*>
        // #<marker at 9 in *scratch*>)

    }

    @Test
    public void setEmptyMatchDataToNil() {
        LispObject data = evaluateString("(set-match-data nil)");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(), data);
    }

    @Test
    public void setNonEmptyMatchDataToNil() {
        evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
        LispObject data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9)), data);
        data = evaluateString("(set-match-data nil)");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(), data);
    }

    @Test
    public void setEmptyMatchDataToWrongStartList() {
        LispObject data = evaluateString("(set-match-data '(a 1 2))");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(), data);
    }

    @Test
    public void setEmptyMatchDataToWrongInMiddleList() {
        try {
            evaluateString("(set-match-data '(1 2 a))");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument interger-or-marker-p a)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();

    }

    @Test
    public void setEmptyMatchDataToWrongList() {
        try {
            evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
            evaluateString("(set-match-data 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp a)", TestSetup.getCause(e));
            LispObject data = evaluateString("(match-data)");
            Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9)), data);
            return;
        }
        Assert.fail();
    }

    @Test
    public void testReplaceMatchInStringNilNil () {
        evaluateString("(set-match-data (list 10 2))");
        try {
            evaluateString("(replace-match \"anna\" nil nil \"qwasqwas\" 10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range -1 -1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMatchStringOutOfStringRange () {
        evaluateString("(set-match-data (list 10 2))");
        try {
            evaluateString("(match-string 0 \"qwasqwas\")");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"qwasqwas\" 10 2)", TestSetup.getCause(e));
//            Assert.assertEquals("'(args-out-of-range 10 2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testReplaceMatchInStringWrong () {
        evaluateString("(set-match-data (list 10 2))");
        try {
            evaluateString("(replace-match \"anna\" nil nil \"qwasqwas\")");
        } catch (Exception e) {
//            Assert.assertEquals("'(args-out-of-range \"qwasqwas\" 10 2)", TestSetup.getCause(e));
            Assert.assertEquals("'(args-out-of-range 10 2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testReplaceMatchInStringLonger () {
        evaluateString("(set-match-data (list 1 3))");
        LispObject replaced = evaluateString("(replace-match \"anna\" nil nil \"qwasqwas\")");
        Assert.assertEquals(new LispString("qannasqwas"), replaced);
    }

    @Test
    public void testReplaceMatchInStringShorter () {
        evaluateString("(set-match-data (list 0 6))");
        LispObject replaced = evaluateString("(replace-match \"anna\" nil nil \"qwasqwas\")");
        Assert.assertEquals(new LispString("annaas"), replaced);
    }

    @Test
    public void testReplaceMatchAsIs() {
        evaluateString("(set-match-data '(0 10))");
        LispObject replaced = evaluateString("(replace-match \"hello\" nil nil \"ThE wORLd6 is Big\")");
        Assert.assertEquals(new LispString("hello is Big"), replaced);
        replaced = evaluateString("(replace-match \"hello\" nil nil \"12345678987654321\")");
        Assert.assertEquals(new LispString("hello7654321"), replaced);
    }

    @Test
    public void testReplaceMatchCapitalized() {
        evaluateString("(set-match-data '(0 10))");
        LispObject replaced = evaluateString("(replace-match \"hello\" nil nil \"THE WORLd6 is Big\")");
        Assert.assertEquals(new LispString("Hello is Big"), replaced);
    }

    @Test
    public void testReplaceMatchUppercase() {
        evaluateString("(set-match-data '(0 10))");
        LispObject replaced = evaluateString("(replace-match \"hello\" nil nil \"THE WORLD6is Big\")");
        Assert.assertEquals(new LispString("HELLOis Big"), replaced);
    }

    @Test
    public void testReplaceMatchWithBackslashLiterally() {
        evaluateString("(set-match-data '(0 10 1 3))");
        LispObject replaced = evaluateString("(replace-match \"he\\llo\" nil t \"1234567890\")");
        Assert.assertEquals(new LispString("he\\llo"), replaced);
    }

    @Test
    public void testReplaceMatchWithBackslashNonLiterally() {
        try {
            evaluateString("(set-match-data '(0 10 1 3))");
            evaluateString("(replace-match \"he\\llo\" nil nil \"1234567890\")");
        } catch (Exception e) {
            Assert.assertEquals("Invalid use of `\\\\' in replacement text", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testReplaceMatchFirstGroup () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\1two\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one12two123"), r);
    }

    @Test
    public void testReplaceMatchSecondGroup () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\2two\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one45two123"), r);
    }

    @Test
    public void testReplaceMatchThirdGroup () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\3two\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01onetwo123"), r);
    }

    @Test
    public void testReplaceMatchOverGroupN () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\45two\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one5two123"), r);
    }

    @Test
    public void testReplaceMatchAmpersand () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\&two\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one23456789two123"), r);
    }

    @Test
    public void testReplaceMatchDoubleAmpersand () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\&two\\&three\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one23456789two23456789three123"), r);
    }

    @Test
    public void testReplaceMatchAmpersandAndNumber () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\&two\\2three\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one23456789two45three123"), r);
    }

    @Test
    public void testReplaceMatchEscapedBackslash () {
        evaluateString("(set-match-data '(2 10 1 3 4 6))");
        LispObject r = evaluateString("(replace-match \"one\\\\two\" nil nil \"0123456789123\")");
        Assert.assertEquals(new LispString("01one\\two123"), r);
    }
}
