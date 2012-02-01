package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingBracketException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingDoubleQuoteException;
import org.jetbrains.emacs4ij.jelisp.exception.UnknownCodeBlockException;
import org.junit.Before;
import org.junit.Ignore;
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
        Assert.assertEquals(LispList.list(), lispObject);
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
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispInteger(5))), lispObject);
    }
    @Test
    public void testComplexList () throws LispException {
        LispObject lispObject = p.parseLine("(5 \"lala\")");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispInteger(5), new LispString("lala"))), lispObject);
    }

    @Test
    public void testStringWithSpaceList () throws LispException {
        LispObject lispObject = p.parseLine("(5 \"la la\")");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispInteger(5), new LispString("la la"))), lispObject);
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
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispSymbol("quote"),  LispList.list(Arrays.<LObject>asList(new LispInteger(5), new LispString("la la"))))), lispObject);
    }

    @Test
    public void testStringWithDblQuote () throws LispException {
        LispObject lispObject = p.parseLine("(5 \"la \\\" la\")");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispInteger(5), new LispString("la \" la"))), lispObject);
    }

    @Test
    public void testInnerList () throws LispException {
        LispObject lispObject = p.parseLine("(5 (10))");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispInteger(5), LispList.list(Arrays.<LObject>asList(new LispInteger(10))))), lispObject);
    }

    @Test
    public void testListWithMultipleSpaces () throws LispException {
        LispObject lispObject = p.parseLine("(   5    \"la   la\"   )");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispInteger(5), new LispString("la   la"))), lispObject);
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
        Assert.assertEquals(LispList.list(), lispObject);
    }

    @Test (expected = MissingClosingBracketException.class)
    public void testUnclosedListWithComments() throws LispException {
        p.parseLine("(5 10 ;); a comment");
    }

    @Test
    public void testEmptyQuote() throws LispException {
        LispObject lispObject = p.parseLine("'");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispSymbol("quote"),  LispSymbol.ourNil)), lispObject);
    }

    @Test
    public void testQuotedSpace() throws LispException {
        LispObject lispObject = p.parseLine("' ");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispSymbol("quote"),  LispSymbol.ourNil)), lispObject);
    }

    @Test
    public void testQuotedInt() throws LispException {
        LispObject lispObject = p.parseLine("'5");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispSymbol("quote"),  new LispInteger(5))), lispObject);
    }

    @Test
    public void testQuotedQuotedList() throws LispException {
        LispObject lispObject = p.parseLine("'(quote 5)");
        Assert.assertEquals(LispList.list(new LispSymbol("quote"), LispList.list(new LispSymbol("quote"), new LispInteger(5))), lispObject);
    }

    @Test
    public void testListOfQuotedItems() {
        LispObject lispObject = p.parseLine("('test)");
        Assert.assertEquals(LispList.list(LispList.list(new LispSymbol("quote"), new LispSymbol("test"))), lispObject);
    }

    @Test
    public void testParsePlus () throws LispException {
        LispObject lispObject = p.parseLine("(+ 2 2)");
        Assert.assertEquals(LispList.list(Arrays.<LObject>asList(new LispSymbol("+"), new LispInteger(2), new LispInteger(2))), lispObject);
    }

    @Test
    public void testParseMultiLineListWithComments () {
        LispObject lispObject = p.parseLine("(rose\nbutterfly;;something like comment\nelefant;;kit\n)");
        Assert.assertEquals(LispList.list(new LispSymbol("rose"), new LispSymbol("butterfly"), new LispSymbol("elefant")), lispObject);
    }

    @Test
    public void testParseMultiLineList () {
        LispObject lispObject = p.parseLine("(rose\nbutterfly\nelefant)");
        Assert.assertEquals(LispList.list(new LispSymbol("rose"), new LispSymbol("butterfly"), new LispSymbol("elefant")), lispObject);
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

    @Test
    public void testParseVector() {
        LispObject lispObject = p.parseLine("[1 \"hello\" anna ['q]]");
        Assert.assertEquals(new LispVector(new LispInteger(1), new LispString("hello"), new LispSymbol("anna"), new LispVector(LispList.list(new LispSymbol("quote"), new LispSymbol("q")))), lispObject);
    }
    
    @Test
    public void testParseCharacter() {
        LispObject c = p.parseLine("?a");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue('a' == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharA() {
        // ?\a ⇒ 7                 ; control-g, C-g
        LispObject c = p.parseLine("?\\a");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(7 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-g");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(7 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^g");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(7 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharB() {
        // ?\b ⇒ 8                 ; backspace, <BS>, C-h
        LispObject c = p.parseLine("?\\b");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(8 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-h");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(8 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^h");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(8 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharT() {
        //?\t ⇒ 9                 ; tab, <TAB>, C-i
        LispObject c = p.parseLine("?\\t");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(9 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-i");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(9 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^i");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(9 == ((LispInteger)c).getData());
    }

    @Test
    public void testParseSpecialCharN() {
        //?\n ⇒ 10                ; newline, C-j
        LispObject c = p.parseLine("?\\n");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(10 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-j");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(10 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^j");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(10 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharV() {
        //?\v ⇒ 11                ; vertical tab, C-k
        LispObject c = p.parseLine("?\\v");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(11 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-k");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(11 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^k");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(11 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharF() {
        //?\f ⇒ 12                ; formfeed character, C-l
        LispObject c = p.parseLine("?\\f");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(12 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-l");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(12 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^l");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(12 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharR() {
        //?\r ⇒ 13                ; carriage return, <RET>, C-m
        LispObject c = p.parseLine("?\\r");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(13 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-m");

        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(13 == ((LispInteger)c).getData());

        c = p.parseLine("?\\^m");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(13 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharE() {
        //?\e ⇒ 27                ; escape character, <ESC>, C-[
        LispObject c = p.parseLine("?\\e");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(27 == ((LispInteger)c).getData());

        c = p.parseLine("?\\C-[");
        //todo: must throw (scan-error "Containing expression ends prematurely" 2150 2150)
        c = p.parseLine("?\\^[");
        //todo: must throw (scan-error "Containing expression ends prematurely" 2149 2149)
    }

    @Test
    public void testParseSpecialCharS() {
        //?\s ⇒ 32                ; space character, <SPC>
        LispObject c = p.parseLine("?\\s");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(32 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharSpace() {
        //?\s ⇒ 32                ; space character, <SPC>
        LispObject c = p.parseLine("? ");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(32 == ((LispInteger)c).getData());
    }

    @Test
    public void testParseSpecialCharBackslash() {
        //?\\ ⇒ 92                ; backslash character, \
        LispObject c = p.parseLine("?\\\\");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(92 == ((LispInteger)c).getData());
    }

    @Ignore
    @Test
    public void testParseSpecialCharDel() {
        //?\d ⇒ 127               ; delete character, <DEL>
        LispObject c = p.parseLine("?\\d");
        Assert.assertTrue(c instanceof LispInteger);
        Assert.assertTrue(127 == ((LispInteger)c).getData());
    }
}

