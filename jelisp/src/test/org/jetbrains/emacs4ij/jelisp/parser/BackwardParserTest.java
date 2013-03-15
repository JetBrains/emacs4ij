package org.jetbrains.emacs4ij.jelisp.parser;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFloat;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.InvalidReadSyntax;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ScanException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;

public class BackwardParserTest {
  private BackwardParser p;

  @Before
  public void setUp() throws Exception {
    p = new BackwardParser();
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
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispInteger(5))), lispObject);
  }
  @Test
  public void testComplexList () throws LispException {
    LispObject lispObject = p.parseLine("(5 \"lala\")");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispInteger(5), new LispString("lala"))), lispObject);
  }

  @Test
  public void testStringWithSpaceList () throws LispException {
    LispObject lispObject = p.parseLine("(5 \"la la\")");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la la"))), lispObject);
  }

  @Test
  public void testMissingClosingBracket () throws LispException {
    LispObject r = p.parseLine("(5 \"la la\"");
    Assert.assertEquals(new LispString("la la"), r);
  }

  @Test
  public void testMissingClosingDblQuote () throws LispException {
    LispObject r = p.parseLine("(5 \"la la");
    Assert.assertEquals(new LispSymbol("la"), r);
  }

  @Test
  public void testQuotedList () throws LispException {
    LispObject lispObject = p.parseLine("'    (5 \"la la\")");
    Assert.assertEquals(LispList.list(new LispInteger(5), new LispString("la la")), lispObject);
  }

  @Test
  public void testStringWithDblQuote () throws LispException {
    LispObject lispObject = p.parseLine("(5 \"la \\\" la\")");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la \" la"))), lispObject);
  }

  @Test
  public void testInnerList () throws LispException {
    LispObject lispObject = p.parseLine("(5 (10))");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispInteger(5), LispList.list(Arrays.<LispObject>asList(new LispInteger(10))))), lispObject);
  }

  @Test
  public void testListWithMultipleSpaces () throws LispException {
    LispObject lispObject = p.parseLine("(   5    \"la   la\"   )");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la   la"))), lispObject);
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
    Assert.assertEquals(new LispSymbol("comment"), lispObject);
  }

  @Test
  public void testIntegerWithComments() throws LispException {
    LispObject lispObject = p.parseLine("5; a comment");
    Assert.assertEquals(new LispSymbol("comment"), lispObject);
  }

  @Test
  public void testUnclosedListWithComments() throws LispException {
    LispObject r = p.parseLine("(5 10 ;); a comment");
    Assert.assertEquals(new LispSymbol("comment"), r);
  }

  @Ignore
  @Test
  public void testEmptyQuote() throws LispException {
    LispObject lispObject = p.parseLine("'");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispSymbol("quote"),  LispSymbol.NIL)), lispObject);
  }

  @Test (expected = EndOfLineException.class)
  public void testQuotedSpace() throws LispException {
    p.parseLine("' ");
  }

  @Test
  public void testQuotedInt() throws LispException {
    LispObject lispObject = p.parseLine("'5");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispSymbol("quote"),  new LispInteger(5))), lispObject);
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
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispSymbol("+"), new LispInteger(2), new LispInteger(2))), lispObject);
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
  public void testParseVectorSimple() {
    LispObject lispObject = p.parseLine("[1 \"hello\" anna [(quote q)]]");
    Assert.assertEquals(new LispVector(new LispInteger(1), new LispString("hello"), new LispSymbol("anna"), new LispVector(LispList.list(new LispSymbol("quote"), new LispSymbol("q")))), lispObject);
  }

  @Test
  public void testParseVector() {
    LispObject lispObject = p.parseLine("[1 \"hello\" anna ['q]]");
    Assert.assertEquals(new LispVector(new LispInteger(1), new LispString("hello"), new LispSymbol("anna"), new LispVector(LispList.list(new LispSymbol("quote"), new LispSymbol("q")))), lispObject);
  }

  @Test
  public void testParseCharacter() {
    LispObject c = p.parseLine("?a");
    Assert.assertEquals(new LispInteger(97), c);
  }

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

  @Test
  public void testParseSpecialCharE() {
    //?\e ⇒ 27                ; escape character, <ESC>, C-[
    LispObject c = p.parseLine("?\\e");
    Assert.assertTrue(c instanceof LispInteger);
    Assert.assertTrue(27 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseSpecialCharS() {
    //?\s ⇒ 32                ; space character, <SPC>
    LispObject c = p.parseLine("?\\s");
    Assert.assertTrue(c instanceof LispInteger);
    Assert.assertTrue(32 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseSpecialCharSpace() {
    //?\s ⇒ 32                ; space character, <SPC>
    LispObject c = p.parseLine("? ");
    Assert.assertEquals(new LispInteger(32), c);
  }

  @Test
  public void testParseSpecialCharBackslash() {
    //?\\ ⇒ 92                ; backslash character, \
    LispObject c = p.parseLine("?\\\\");
    Assert.assertTrue(c instanceof LispInteger);
    Assert.assertTrue(92 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseSpecialCharDel() {
    //?\d ⇒ 127               ; delete character, <DEL>
    LispObject c = p.parseLine("?\\d");
    Assert.assertTrue(c instanceof LispInteger);
    Assert.assertTrue(127 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseBackQuoteSimple() {
    LispObject c = p.parseLine("`()");
    Assert.assertEquals("(\\` nil)", c.toString());
  }

  @Test
  public void testParseBackQuoteComma() {
    LispObject c = p.parseLine("`(1 ,2)");
    Assert.assertEquals("(\\` (1 (\\, 2)))", c.toString());
  }

  @Test
  public void testParseBackQuoteDog() {
    LispObject c = p.parseLine("`(1 ,2 ,@3)");
    Assert.assertEquals("(\\` (1 (\\, 2) (\\,@ 3)))", c.toString());
  }

  @Test
  public void testParseBackQuoteNested() {
    LispObject c = p.parseLine("`(1 ,2 ,@`3)");
    Assert.assertEquals("(\\` (1 (\\, 2) (\\,@ (\\` 3))))", c.toString());
  }

  @Test
  public void testParseDot() {
    try {
      p.parseLine("(. . 2)");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \".\")", e.getMessage());
      return;
    }
    Assert.fail();
  }

  @Test
  public void testParseDot1() {
    try {
      p.parseLine("(.\n. 2)");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \".\")", e.getMessage());
      return;
    }
    Assert.fail();
  }

  @Test
  public void testParseDot2() {
    LispObject r = p.parseLine("(. 2)");
    Assert.assertEquals(new LispInteger(2), r);
  }

  @Test
  public void testParseDot3() {
    try {
      p.parseLine("(. 2 3)");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \". in wrong context\")", e.getMessage());
      return;
    }
    Assert.fail();
  }

  @Test
  public void testParseDot40() {
    LispObject r = p.parseLine("(1 2 . 3)");
    Assert.assertEquals(LispList.testList(new LispInteger(1), new LispInteger(2), new LispInteger(3)), r);
  }

  @Test
  public void testParseDot4() {
    LispObject r = p.parseLine("(1 . 2)");
    Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), r);
  }

  @Test
  public void testParseDot5() {
    try {
      p.parseLine("(1 . 2 3)");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \". in wrong context\")", e.getMessage());
      return;
    }
    Assert.fail();
  }

//    @Test
//    public void testParseDot6() {
//        LispObject r = p.parseLine("(.. 2)");
//        Assert.assertEquals(LispList.list(new LispSymbol("\\.\\."), new LispInteger(2)), r);
//    }
//
//    @Test
//    public void testParseDot7() {
//        LispObject r = p.parseLine("(.)");
//        Assert.assertEquals(LispList.list(new LispSymbol("\\.")), r);
//        r = p.parseLine("( .)");
//        Assert.assertEquals(LispList.list(new LispSymbol("\\.")), r);
//    }

  @Test
  public void testParseDot8() {
    try {
      p.parseLine("(. )");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \")\")", e.getMessage());
      return;
    }
    Assert.fail();
  }

  @Test
  public void testSymbol1() {
    LispObject symbol = p.parseLine("\\\\a");
    Assert.assertEquals(new LispSymbol("\\\\a"), symbol);
  }

//    @Test
//    public void testSymbol2() {
//        LispObject symbol = p.parseLine("\\a");
//        Assert.assertEquals(new LispSymbol("a"), symbol);
//    }
//
//    @Test
//    public void testSymbol3() {
//        LispObject symbol = p.parseLine("\\.a");
//        Assert.assertEquals(new LispSymbol("\\.a"), symbol);
//    }
//
//    @Test
//    public void testSymbol4() {
//        LispObject symbol = p.parseLine("b\\\\a");
//        Assert.assertEquals(new LispSymbol("b\\\\a"), symbol);
//    }
//
//    @Test
//    public void testSymbol5() {
//        LispObject symbol = p.parseLine("b\\a");
//        Assert.assertEquals(new LispSymbol("ba"), symbol);
//    }
//
//    @Test
//    public void testSymbol6() {
//        LispObject symbol = p.parseLine("b\\.a");
//        Assert.assertEquals(new LispSymbol("b\\.a"), symbol);
//    }
//
//    @Test
//    public void testSymbol7() {
//        LispObject symbol = p.parseLine("b.a");
//        Assert.assertEquals(new LispSymbol("b\\.a"), symbol);
//    }
//
//    @Test
//    public void testSymbol8() {
//        LispObject symbol = p.parseLine(".a");
//        Assert.assertEquals(new LispSymbol("\\.a"), symbol);
//    }
//
//    @Test
//    public void testSymbol9() {
//        LispObject symbol = p.parseLine("b\\\\");
//        Assert.assertEquals(new LispSymbol("b\\\\"), symbol);
//    }
//
//    @Test
//    public void testSymbol10() {
//        LispObject symbol = p.parseLine("b\\");
//        Assert.assertEquals(new LispSymbol("b"), symbol);
//    }
//
//    @Test
//    public void testSymbol11() {
//        LispObject symbol = p.parseLine("b\\.");
//        Assert.assertEquals(new LispSymbol("b\\."), symbol);
//    }
//
//    @Test
//    public void testSymbol12() {
//        LispObject symbol = p.parseLine("b.");
//        Assert.assertEquals(new LispSymbol("b\\."), symbol);
//    }
//
//    @Test
//    public void testSymbol13() {
//        LispObject symbol = p.parseLine("b.,?a");
//        Assert.assertEquals(new LispSymbol("b\\.\\,\\?a"), symbol);
//    }

  @Test
  public void testParseConsWithCommentsOk() {
    LispObject r = p.parseLine("(1 . ;;ololo\n;;hello\n2)");
    Assert.assertEquals(LispList.cons(new LispInteger(1), new LispInteger(2)), r);
  }

  @Test
  public void testParseConsWithCommentsFail() {
    try {
      p.parseLine("'(1 . ;;ololo\n)");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \")\")", e.getMessage());
      return;
    }
    Assert.fail();
  }

  @Test (expected = ScanException.class)
  public void testParseSpecialCharWrong1() {
    p.parseLine("?\\C-[");
  }

  @Test (expected = ScanException.class)
  public void testParseSpecialCharWrong2() {
    p.parseLine("?\\^(");
  }

  @Ignore
  @Test (expected = InvalidReadSyntax.class)
  //todo: comes ScanException '(scan-error Unbalanced parentheses), BackwardParser.java:79
  public void testParseSpecialCharWrong3() {
    p.parseLine("?\\^\\\"");
  }

  @Test (expected = ScanException.class)
  public void testParseSpecialCharWrong4() {
    p.parseLine("?\\^\\\\\"");
  }

  @Test
  public void testParseStringWithTextPropNoProp() {
    Assert.assertEquals(new LispString("hello"),  p.parseLine("#(\"hello\")"));
  }
}
