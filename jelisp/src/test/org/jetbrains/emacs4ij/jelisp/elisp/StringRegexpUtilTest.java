package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.subroutine.Match;
import org.junit.Ignore;
import org.junit.Test;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
        String s = StringRegexpUtil.transformEmacsRegexpToJava(null, "\\\\'", "hei");
        Assert.assertEquals("\\z", s);
    }

    @Test
    public void testTransformToRegexpClassBegin() {
        String s = StringRegexpUtil.transformEmacsRegexpToJava(null, "\\\\`", "hei");
        Assert.assertEquals("\\A", s);
    }

    @Test
    public void testQuoteOpeningSquareBracketInSquareBrackets() {
        String s = StringRegexpUtil.quoteOpeningSquareBracketInSquareBrackets("hello[wow[how[are]you[fine[hm]ok");
        Assert.assertEquals("hello[wow\\[how\\[are]you[fine\\[hm]ok", s);
    }

    @Test
    public void testTransform() {
        String s = StringRegexpUtil.transformEmacsRegexpToJava(null, "\\\\\\\\[{[]", "");
        Assert.assertEquals("\\\\\\\\[\\{\\[]", s);
    }

    @Ignore
    @Test
    public void testWordBound() {
        StringRegexpUtil.match(null, "\\b", ";word bound", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(0)), data);
    }

    @Ignore
    @Test
    public void testWordContentBound() {
        StringRegexpUtil.match(null, "\\bword\\b", "word bound", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(4)), data);
    }

    @Ignore
    @Test
    public void test() {
        String ofType = "wordbunk";
        String notType = "; ";
        String regexp = "(([" + ofType + "](\\z|[" + notType + "]))|((\\A|[" + notType + "])([" + ofType + "]))|(\\A\\z))";
        Pattern p = Pattern.compile(regexp);
        Pattern.compile("[; ][abc]");
        Matcher m  = p.matcher(";word boundk");
        int from = 0;
        while (m.find(from)) {
            System.out.println(m.start() + "; " + m.end() + " = " + m.group());
            from = m.end();
            if (from - 1 > m.start())
                from--;
        }
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
        StringRegexpUtil.match(null, "\\S.", "not word", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(1)), data);
    }

    @Test
    public void testQuotedSpecialChar() {
        StringRegexpUtil.match(null, "\\\\s.", "\\s.", 0, false);
        LispList data = Match.matchData(null, null, null);
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(3)), data);
    }
}
