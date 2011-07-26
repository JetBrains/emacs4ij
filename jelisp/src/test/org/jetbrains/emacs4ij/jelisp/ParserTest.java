package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingBracketException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingDoubleQuoteException;
import org.jetbrains.emacs4ij.jelisp.exception.UnknownCodeBlockException;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/12/11
 * Time: 1:01 PM
 * To change this template use File | Settings | File Templates.
 */
public class ParserTest {
    private Parser p;

    @Before
    public void setUp() throws Exception {
        p = new Parser();
    }

    @Test
    public void testEmptyList () throws LispException {
        LispObject lispObject = p.parseLine("()");
        Assert.assertEquals(new LispList(), lispObject);
    }
    @Test
    public void testEmptyString() throws LispException {
        LispObject lispObject = p.parseLine("\"\"");
        Assert.assertEquals(new LispString(""), lispObject);
    }
    @Test
    public void testString() throws LispException {
        LispObject lispObject = p.parseLine("\"hello\"");
        Assert.assertEquals(new LispString("hello"), lispObject);
    }

    @Test
    public void testNumber() throws LispException {
        LispObject lispObject = p.parseLine("5");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }
    @Test
    public void testList () throws LispException {
        LispObject lispObject = p.parseLine("(5)");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispInteger(5))), lispObject);
    }
    @Test
    public void testComplexList () throws LispException {
        LispObject lispObject = p.parseLine("(5 \"lala\")");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispInteger(5), new LispString("lala"))), lispObject);
    }

    @Test
    public void testStringWithSpaceList () throws LispException {
        LispObject lispObject = p.parseLine("(5 \"la la\")");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la la"))), lispObject);
    }

    @Test(expected = MissingClosingBracketException.class)
    public void testMissingClosingBracket () throws LispException {
        p.parseLine("(5 \"la la\"");
    }

    @Test(expected = MissingClosingDoubleQuoteException.class)
    public void testMissingClosingDblQuote () throws LispException {
        p.parseLine("(5 \"la la");
    }

    @Test (expected = UnknownCodeBlockException.class)
    public void testQuotedList () throws LispException {
        LispObject lispObject = p.parseLine("'    (5 \"la la\")");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispSymbol("quote"),  new LispList(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la la"))))), lispObject);
    }

    @Test
    public void testStringWithDblQuote () throws LispException {
        LispObject lispObject = p.parseLine("(5 \"la \\\" la\")");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la \" la"))), lispObject);
    }

    @Test
    public void testInnerList () throws LispException {
        LispObject lispObject = p.parseLine("(5 (10))");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispInteger(5), new LispList(Arrays.<LispObject>asList(new LispInteger(10))))), lispObject);
    }

    @Test
    public void testListWithMultipleSpaces () throws LispException {
        LispObject lispObject = p.parseLine("(   5    \"la   la\"   )");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la   la"))), lispObject);
    }

    @Test
    public void testFloatNegativeInfinity() throws LispException {
        LispObject lispObject = p.parseLine("-1.0e+INF");
        Assert.assertEquals(LispFloat.ourNegativeInfinity, lispObject);
    }

    @Test
    public void testFloatPositiveInfinity() throws LispException {
        LispObject lispObject = p.parseLine("1.0e+INF");
        Assert.assertEquals(LispFloat.ourPositiveInfinity, lispObject);
    }

    @Test
    public void testFloatNanNoSign() throws LispException {
        LispObject lispObject = p.parseLine("0.0e+NaN");
        Assert.assertEquals(LispFloat.ourNaN, lispObject);
    }

    @Test
    public void testFloatNanWithSign() throws LispException {
        LispObject lispObject = p.parseLine("-0.0e+NaN");
        Assert.assertEquals(LispFloat.ourNaN, lispObject);
    }

    /*@Test
    public void testFloatNanEquality() throws LispException {
        LispObject lispObject = p.parseLine("-0.0e+NaN");
        Assert.assertEquals(LispFloat.ourNaN, lispObject);
    }*/

    @Test
    public void testFloat1() throws LispException {
        LispObject lispObject = p.parseLine("1500.0");
        Assert.assertEquals(new LispFloat(1500), lispObject);
    }

    @Test
    public void testFloat2() throws LispException {
        LispObject lispObject = p.parseLine("15e2");
        Assert.assertEquals(new LispFloat(1500), lispObject);
    }

    @Test
    public void testFloat3() throws LispException {
        LispObject lispObject = p.parseLine("15.0e2");
        Assert.assertEquals(new LispFloat(1500), lispObject);
    }

    @Test
    public void testFloat4() throws LispException {
        LispObject lispObject = p.parseLine("1.5e3");
        Assert.assertEquals(new LispFloat(1500), lispObject);
    }

    @Test
    public void testFloat5() throws LispException {
        LispObject lispObject = p.parseLine(".15e4");
        Assert.assertEquals(new LispFloat(1500), lispObject);
    }

    @Test
    public void testEmptyLineWithComments() throws LispException {
        LispObject lispObject = p.parseLine("; a comment");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testIntegerWithComments() throws LispException {
        LispObject lispObject = p.parseLine("5; a comment");
        Assert.assertEquals(new LispInteger(5), lispObject);
    }

    @Test
    public void testStringWithComments() throws LispException {
        LispObject lispObject = p.parseLine("\"test;\"; a comment");
        Assert.assertEquals(new LispString("test;"), lispObject);
    }

    @Test
    public void testEmptyListWithComments() throws LispException {
        LispObject lispObject = p.parseLine("(); a comment");
        Assert.assertEquals(new LispList(), lispObject);
    }

    @Test (expected = MissingClosingBracketException.class)
    public void testUnclosedListWithComments() throws LispException {
        p.parseLine("(5 10 ;); a comment");
    }

    @Test
    public void testEmptyQuote() throws LispException {
        LispObject lispObject = p.parseLine("'");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispSymbol("quote"),  LispSymbol.ourNil)), lispObject);
    }

    @Test
    public void testQuotedSpace() throws LispException {
        LispObject lispObject = p.parseLine("' ");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispSymbol("quote"),  LispSymbol.ourNil)), lispObject);
    }

    @Test
    public void testQuotedInt() throws LispException {
        LispObject lispObject = p.parseLine("'5");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispSymbol("quote"),  new LispInteger(5))), lispObject);
    }

    @Test
    public void testQuotedQuotedList() throws LispException {
        LispObject lispObject = p.parseLine("'(quote 5)");
        Assert.assertEquals(new LispList(new LispSymbol("quote"), new LispList(new LispSymbol("quote"), new LispInteger(5))), lispObject);
    }

    @Test
    public void testListOfQuotedItems() {
        LispObject lispObject = p.parseLine("('test)");
        Assert.assertEquals(new LispList(new LispList(new LispSymbol("quote"), new LispSymbol("test"))), lispObject);
    }

    @Test
    public void testParsePlus () throws LispException {
        LispObject lispObject = p.parseLine("(+ 2 2)");
        Assert.assertEquals(new LispList(Arrays.<LispObject>asList(new LispSymbol("+"), new LispInteger(2), new LispInteger(2))), lispObject);
    }

    @Test
    public void testParseMultiLineListWithComments () {
        LispObject lispObject = p.parseLine("(rose\nbutterfly;;something like comment\nelefant;;kit\n)");
        Assert.assertEquals(new LispList(new LispSymbol("rose"), new LispSymbol("butterfly"), new LispSymbol("elefant")), lispObject);
    }

    @Test
    public void testParseMultiLineList () {
        LispObject lispObject = p.parseLine("(rose\nbutterfly\nelefant)");
        Assert.assertEquals(new LispList(new LispSymbol("rose"), new LispSymbol("butterfly"), new LispSymbol("elefant")), lispObject);
    }

    @Test
    public void testParseMultiLineString () {
        LispObject lispObject = p.parseLine("\"multiline\nstring\"");
        Assert.assertEquals(new LispString("multiline\nstring"), lispObject);
    }

    @Test
    public void testParseSymbol() throws LispException {
        LispObject lispObject = p.parseLine("test");
        Assert.assertEquals(new LispSymbol("test"), lispObject);
    }


}

