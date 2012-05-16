package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 26/02/12
 * Time: 18:19
 * To change this template use File | Settings | File Templates.
 */
public class BStringTest extends BaseSubroutineTest {
    @Test
    public void testFormatEmpty() {
        LispObject s = evaluateString("(format \"\")");
        Assert.assertEquals(new LispString(""), s);
    }

    @Test
    public void testFormatNoNeedInArgs() {
        LispObject s = evaluateString("(format \"hello\" 1 2 3)");
        Assert.assertEquals(new LispString("hello"), s);
    }

    @Test
    public void testFormatNotEnoughArgs() {
        try {
            evaluateString("(format \"%d\")");
        } catch (Exception e) {
            Assert.assertEquals("Not enough arguments for format string", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testFormatUnexpectedEndOfFormatString() {
        try {
            evaluateString("(format \"%123456\" 1)");
        } catch (Exception e) {
            Assert.assertEquals("Format string ends in middle of format specifier", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testFormatInvalidFormatCharacter() {
        try {
            evaluateString("(format \"%123456q\" 1)");
        } catch (Exception e) {
            Assert.assertEquals("Invalid format operation %q", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testFormatSimple() {
        LispObject s = evaluateString("(format \"%12d\" 1 2 3)");
        Assert.assertEquals(new LispString("           1"), s);
    }

    @Test
    public void testFormatO() {
        LispObject s = evaluateString("(format \"%o\" 123)");
        Assert.assertEquals(new LispString("173"), s);
    }

    @Test
    public void testFormatX() {
        LispObject s = evaluateString("(format \"%x\" 123)");
        Assert.assertEquals(new LispString("7b"), s);
        s = evaluateString("(format \"%X\" 123)");
        Assert.assertEquals(new LispString("7B"), s);
    }

    @Test
    public void testFormatH() {
        try {
            evaluateString("(format \"%h\" 123)");
        } catch (Exception e) {
            Assert.assertEquals("Invalid format operation %h", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testStringMatchSimple() {
        LispObject r = evaluateString("(string-match \"a\" \"africa\")");
        Assert.assertEquals(new LispInteger(0), r);
        evaluateString("(setq case-fold-search t)");
        r = evaluateString("(string-match \"A\" \"africa\" 1)");
        Assert.assertEquals(new LispInteger(5), r);
        evaluateString("(setq case-fold-search nil)");
        r = evaluateString("(string-match \"A\" \"africa\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testStringMatchOutOfRange() {
        try {
            evaluateString("(string-match \"a\" \"africa\" 10)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"africa\" 10)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testStringMatchTick() {
        LispObject r = evaluateString("(string-match \"^[ACHMsS]-.\" \"M-x\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"^[ACHMsS]-.\" \"kvaM-x\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testStringMatchTickAfterNewLine() {
        LispObject r = evaluateString("(string-match \"^[ACHMsS]-.\" \"kva\nM-x\")");
        Assert.assertEquals(new LispInteger(4), r);
    }

    @Test
    public void testStringMatchCharAlternative() {
        LispObject r = evaluateString("(string-match \"[abc]\" \"d\\^alla\")");
        Assert.assertEquals(new LispInteger(3), r);
    }

    @Test
    public void testStringMatchStar() {
        LispObject r = evaluateString("(string-match \"hi*\" \"hihihi5\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"a*\" \"sdf\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatchBackSlash() {
        LispObject r = evaluateString("(string-match \"a\\\\\\\\.b\" \"a\\\\.b\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatchQuestion() {
        LispObject r = evaluateString("(string-match \"caca?r\" \"cacr\")");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(string-match \"a?\" \"crtcr\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatchDollar() {
        LispObject r = evaluateString("(string-match \"u?hhhh$\" \"ah\")");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(string-match \"u?h$\" \"ah\")");
        Assert.assertEquals(new LispInteger(1), r);
    }

    @Test
    public void testStringMatchDot() {
        LispObject r = evaluateString("(string-match \"[.]\" \"a.\")");
        Assert.assertEquals(new LispInteger(1), r);
        r = evaluateString("(string-match \".\" \"a.\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testStringMatch() {
        LispObject r = evaluateString("(string-match \"^[ACHMsS]-.\" \"M-x\")");
        Assert.assertEquals(new LispInteger(0), r);
    }

    @Test
    public void testCapitalize() {
        LispObject r = evaluateString("?a");
        Assert.assertEquals(new LispInteger(97), r);
        r = evaluateString("(capitalize ?a)");
        Assert.assertEquals(new LispInteger(65), r);
        r = evaluateString("(capitalize 97)");
        Assert.assertEquals(new LispInteger(65), r);
        r = evaluateString("(capitalize 5)");
        Assert.assertEquals(new LispInteger(5), r);
        r = evaluateString("(capitalize \"EVERY day and\")");
        Assert.assertEquals(new LispString("Every Day And"), r);
    }

    @Test
    public void testSubstringOutOfRange1() {
        try {
            evaluateString("(substring \"hello\" -6 5)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"hello\" -1 5)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSubstringOutOfRange2() {
        try {
            evaluateString("(substring \"hello\" 1 7)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"hello\" 1 7)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSubstringOutOfRange3() {
        try {
            evaluateString("(substring \"hello\" 0 -6)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"hello\" 0 -1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSubstringOutOfRange4() {
        try {
            evaluateString("(substring \"hello\" 3 2)");
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"hello\" 3 2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testSubstring() {
        LispObject r = evaluateString("(substring \"hello\" 3 3)");
        Assert.assertEquals(new LispString(""), r);
        r = evaluateString("(substring \"hello\" -1)");
        Assert.assertEquals(new LispString("o"), r);
        r = evaluateString("(substring \"hello\" 0 4)");
        Assert.assertEquals(new LispString("hell"), r);
    }

    @Test
    public void testToNumber() {
        LispObject n = evaluateString("(string-to-number \"0.0110101\" 10)");
        Assert.assertEquals(new LispFloat(0.0110101), n);
        n = evaluateString("(string-to-number \"0.0110101\")");
        Assert.assertEquals(new LispFloat(0.0110101), n);
        n = evaluateString("(string-to-number \"0.0110101\" nil)");
        Assert.assertEquals(new LispFloat(0.0110101), n);
        n = evaluateString("(string-to-number \"0.0110101\" 2)");
        Assert.assertEquals(new LispInteger(0), n);
        n = evaluateString("(string-to-number \"110101\" 2)");
        Assert.assertEquals(new LispInteger(53), n);
    }

    @Test
    public void testReadFromString() {
        LispList read = BString.readFromString(new LispString("a"), null, null);
        Assert.assertEquals(LispList.cons(new LispSymbol("a"), new LispInteger(1)), read);
    }

    @Test
    public void testReadFromStringError() {
        try {
            BString.readFromString(new LispString("a"), new LispInteger(2), null);
        } catch (Exception e) {
            Assert.assertEquals("'(args-out-of-range \"a\" 2 1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testRegexpQuote() {
        LispObject regexp = evaluateString("(regexp-quote \"^hello$\")");
        Assert.assertEquals(new LispString("\\\\^hello\\\\$"), regexp);
    }

    @Test
    public void testStringMatchBackslashAlternative() {
        LispObject match = evaluateString("(string-match \"foo\\\\|bar\" \"bar\")");
        Assert.assertEquals(new LispInteger(0), match);
        match = evaluateString("(string-match \"foo\\\\|bar\" \"foobar\")");
        Assert.assertEquals(new LispInteger(0), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(3)), matchData);
        match = evaluateString("(string-match \"foo\\\\|\\\\(bar*q\\\\)k\" \"barrrqk\")");
        Assert.assertEquals(new LispInteger(0), match);
    }

    @Test
    public void testStringMatchBrackets() {
        Assert.assertEquals(new LispInteger(0), evaluateString("(string-match \"ba\\\\(na\\\\)*\" \"banana\")"));
    }

    @Test
    public void testStringMatchShyGroup() {
        evaluateString("(string-match \"\\\\(?:foo\\\\|bar*q\\\\)\" \"qbarrrrrrq\")");
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(10)), matchData);

        evaluateString("(string-match \"\\\\(foo\\\\|bar*q\\\\)\" \"qbarrrrrrq\")");
        matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(10), new LispInteger(1), new LispInteger(10)),
                matchData);

        evaluateString("(string-match \"\\\\(?:he\\\\|lo\\\\)\" \"hello world\")");
        matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(2)), matchData);
    }

    @Test
    public void testRoundBracketAlternative() {
        Assert.assertEquals(new LispInteger(0), evaluateString("(string-match \"\\\\(b(ar\\\\)\" \"b(ar\")"));
    }

    @Test
    public void testStringMatchBackReferenceFromPrevGroup() {
        LispObject match = evaluateString("(string-match \"\\\\(bar*q\\\\|hi\\\\)anna\\\\1\\\\(test\\\\1\\\\)\" \"hiannahitesthi\")");
        Assert.assertEquals(new LispInteger(0), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(14), new LispInteger(0), new LispInteger(2),
                new LispInteger(8), new LispInteger(14)), matchData);
    }

    @Test
    public void testStringMatchBackReference() {
        LispObject match = evaluateString("(string-match \"\\\\(bar*q\\\\)anna\\\\1\" \"qbarrrrrrqannabarrrrrrq\")");
        Assert.assertEquals(new LispInteger(1), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(23), new LispInteger(1), new LispInteger(10)),
                matchData);
    }

    @Test
    public void testStringMatchBackReferenceNoMatch() {
        LispObject match = evaluateString("(string-match \"\\\\(bar*q\\\\)anna\\\\1\" \"qbarrrrrrqannabarrrrrq\")");
        Assert.assertEquals(LispSymbol.ourNil, match);
    }

    @Test
    public void testStringMatchBackReferenceInvalid() {
        try {
            evaluateString("(string-match \"\\\\(bar*q\\\\)anna\\\\2\" \"qbarrrrrrqannabarrrrrrq\")");
        } catch (Exception e) {
            Assert.assertEquals("'(invalid-regexp \"Invalid back reference\")", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testStringMatchBackReferenceInvalidSimple() {
        try {
            evaluateString("(string-match \"\\\\2\" \"\\\\2\")");
        } catch (Exception e) {
            Assert.assertEquals("'(invalid-regexp \"Invalid back reference\")", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testStringMatchZeroNotRef() {
        Assert.assertEquals(new LispInteger(1), evaluateString("(string-match \"\\\\0\" \"\\\\0\")"));
    }

    @Test
    public void testStringMatchCannotBackRefShyGroup() {
        LispObject match = evaluateString("(string-match \"\\\\(he\\\\)\\\\(?:ek\\\\|ll\\\\)\\\\1\" \"qhellheo\")");
        Assert.assertEquals(new LispInteger(1), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(7), new LispInteger(1), new LispInteger(3)),
                matchData);
    }

    @Test
    public void testMatchEnd() {
        LispObject match = evaluateString("(string-match \"\\\\'\" \"hei\")");
        Assert.assertEquals(new LispInteger(3), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(3)), matchData);
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(string-match \"\\\\'hei\" \"hei\")"));
        Assert.assertEquals(LispList.list(), evaluateString("(match-data)"));
    }

    @Test
    public void testMatchBeginning() {
        LispObject match = evaluateString("(string-match \"\\\\`\" \"hei\")");
        Assert.assertEquals(new LispInteger(0), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(0)), matchData);
    }

    @Test
    public void testMatchBeginningAndEnd() {
        LispObject match = evaluateString("(string-match \"\\\\`\\\\'\" \"\")");
        Assert.assertEquals(new LispInteger(0), match);
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(0)), matchData);
    }

    @Test
    public void testStringMatchStrange () {
        evaluateString("(string-match \"\\\\[{[]\" \"\\{\")");
        LispObject matchData = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(2)), matchData);
    }

    @Test
    public void testStringLessP() {
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(string-lessp 'a \"b\")"));
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(string-lessp 'b \"b\")"));
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(string-lessp 'c \"b\")"));
    }

    @Test
    public void testSubstringWithTextProps() {
        evaluateString("(setq s (propertize \"hello\" 'a 'b))");
        LispObject o = evaluateString("(substring s 2 4)");
        LispString expected = new LispString("ll");
        expected.actOnTextProperties(0, 2, LispList.list(new LispSymbol("a"), new LispSymbol("b")),
                TextPropertiesInterval.Action.ADD);
        Assert.assertEquals(expected, o);
    }

    @Test
    public void testSubstringNoTextProps() {
        evaluateString("(setq s (propertize \"hello\" 'a 'b))");
        LispObject o = evaluateString("(substring-no-properties s 2 4)");
        Assert.assertEquals( new LispString("ll"), o);
    }

    @Test
    public void testSubstringVector() {
        evaluateString("(defvar a '[1 2 3 4 5])");
        evaluateString("(defvar b (substring a 2 4))");
        LispObject r = evaluateString("b");
        Assert.assertEquals(new LispVector(new LispInteger(3), new LispInteger(4)), r);
        evaluateString("(aset b 0 5)");
        r = evaluateString("b");
        Assert.assertEquals(new LispVector(new LispInteger(5), new LispInteger(4)), r);
        r = evaluateString("a");
        Assert.assertEquals(new LispVector(new LispInteger(1), new LispInteger(2), new LispInteger(3),
                new LispInteger(4), new LispInteger(5)), r);
    }
}
