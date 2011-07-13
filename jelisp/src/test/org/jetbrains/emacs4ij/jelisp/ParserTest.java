package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingBracketException;
import org.jetbrains.emacs4ij.jelisp.exception.MissingClosingDoubleQuoteException;
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
        Assert.assertEquals(Environment.ourNilSymbol, lispObject);
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

    @Test
    public void testQuotedList () throws LispException {
        LispObject lispObject = p.parseLine("'(5 \"la la\")");
        Assert.assertEquals(new LispString("(5 \"la la\")"), lispObject);
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


}
