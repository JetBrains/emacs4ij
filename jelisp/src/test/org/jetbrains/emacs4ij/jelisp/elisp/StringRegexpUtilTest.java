package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.subroutine.Match;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/2/12
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class StringRegexpUtilTest {
    @Test
    public void testInvertQuotation() {
        Assert.assertEquals("(ba\\(r)", StringRegexpUtil.invertQuotation("\\\\(ba(r\\\\)", "()"));
    }

    @Test
    public void testInvertQuotation2() {
        Assert.assertEquals("\\\\(ba\\(r)", StringRegexpUtil.invertQuotation("\\\\\\(ba(r\\\\)", "()"));
    }

    @Test
    public void testInvertQuotation3() {
        Assert.assertEquals("\\\\(ba\\(r)", StringRegexpUtil.invertQuotation("\\\\\\\\(ba(r\\\\)", "()"));
    }

    @Test
    public void testInvertQuotation4() {
        Assert.assertEquals("(qu)(ick)", StringRegexpUtil.invertQuotation("\\\\(qu\\\\)\\\\(ick\\\\)", "()"));
    }

    @Test
    public void testGetNthMatchAlternative() {
        String s = StringRegexpUtil.getNthMatchAlternative("(wow)(te*st)\\\\1", 1, 13, "wow teeest hello");
        Assert.assertEquals("teeest", s);
    }

    @Test
    public void testTransformToRegexpClassEnd() {
        StringRegexpUtil.transformEmacsRegexpToJava(null, "\\\\'", "hei");
        Assert.assertEquals("\\z", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testTransformToRegexpClassBegin() {
        StringRegexpUtil.transformEmacsRegexpToJava(null, "\\\\`", "hei");
        Assert.assertEquals("\\A", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testQuoteOpeningSquareBracketInSquareBrackets() {
        String s = StringRegexpUtil.quoteOpeningSquareBracketInSquareBrackets("hello[wow[how[are]you[fine[hm]ok");
        Assert.assertEquals("hello[wow\\[how\\[are]you[fine\\[hm]ok", s);
    }

    @Test
    public void testTransform() {
        StringRegexpUtil.transformEmacsRegexpToJava(null, "\\\\\\\\[{[]", "");
        Assert.assertEquals("\\\\\\\\[\\{\\[]", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testWordContentBound() {
        String dataString = StringRegexpUtil.markBounds(null, "\\b", "word bound");
        Assert.assertEquals("1word2 1bound2", dataString);
        StringRegexpUtil.match(null, "\\bword\\b", "word bound", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(4)), data);
    }

    @Test
    public void testWord() {
        StringRegexpUtil.match(null, "\\w", "word", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(1)), data);
    }

    @Test
    public void testNotWord() {
        StringRegexpUtil.match(null, "\\W", "not word", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(4)), data);
    }

    @Test
    public void testNotWord2() {
        StringRegexpUtil.match(null, "\\Sw", "not word", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(4)), data);
    }

    @Test
    public void testNotWordNil() {
        Match.setMatchData(LispList.list(new LispInteger(1000)), null);
        StringRegexpUtil.match(null, "\\W", "word", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(), data);
    }

    @Test
    public void testPunctuationClass() {
        StringRegexpUtil.match(null, "\\s.", "word?", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(5)), data);
    }

    @Test
    public void testNotPunctuationClass() {
        StringRegexpUtil.match(null, "\\S.", "not word?", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(1)), data);
    }

    @Test
    public void testQuotedSpecialChar() {
        StringRegexpUtil.match(null, "\\\\s.", "\\s.", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(3)), data);
    }

    @Test
    public void testMarkWordStart() {
        String data = StringRegexpUtil.markBounds(null, "\\<", "word start");
        Assert.assertEquals("1word 1start", data);
        Assert.assertEquals("1", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testMarkWordEnd() {
        String data = StringRegexpUtil.markBounds(null, "\\>", "word start");
        Assert.assertEquals("word2 start2", data);
        Assert.assertEquals("2", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testMarkSymbolStart() {
        String data = StringRegexpUtil.markBounds(null, "\\_<", "symbol _start");
        Assert.assertEquals("3symbol 3_start", data);
        Assert.assertEquals("3", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testMarkSymbolEnd() {
        String data = StringRegexpUtil.markBounds(null, "\\_>", "symbol_ end");
        Assert.assertEquals("symbol_4 end4", data);
        Assert.assertEquals("4", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testMarkWordBounds() {
        String data = StringRegexpUtil.markBounds(null, "\\b", "word start");
        Assert.assertEquals("1word2 1start2", data);
        Assert.assertEquals("[012]", StringRegexpUtil.myTransformedRegex);
    }

    @Test
    public void testMatchWordStart() {
        StringRegexpUtil.match(null, "\\<", "word start", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(0)), data);
    }

    @Test
    public void testMatchWordEnd() {
        StringRegexpUtil.match(null, "\\>", "word start", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(4)), data);
    }

    @Test
    public void testMatchWordEnd2() {
        StringRegexpUtil.match(null, "\\>", "$bound", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(6), new LispInteger(6)), data);
    }

    @Test
    public void testMatchWordBound() {
        StringRegexpUtil.match(null, "\\b", "$bound", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(0)), data);
    }

    @Test
    public void testMatchEmptyWordBounds() {
        StringRegexpUtil.match(null, "\\b", "", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(0)), data);
        Assert.assertEquals(-1, StringRegexpUtil.match(null, "\\<", "", 0, false));
        Assert.assertEquals(-1, StringRegexpUtil.match(null, "\\>", "", 0, false));
    }

    @Test
    public void testMatchNotWordBounds() {
        Assert.assertEquals(-1, StringRegexpUtil.match(null, "\\B", "", 0, false));
        Assert.assertEquals(-1, StringRegexpUtil.match(null, "\\B", "a", 0, false));
        StringRegexpUtil.match(null, "\\B", "ab", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(1)), data);
    }
}
