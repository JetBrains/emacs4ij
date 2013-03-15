package org.jetbrains.emacs4ij.jelisp.parser;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFloat;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.text.Action;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.MissingClosingBracketException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.MissingClosingDoubleQuoteException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ScanException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;

public class ForwardParserTest {
  private ForwardParser p;

  @Before
  public void setUp() throws Exception {
    p = new ForwardParser();
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
    Assert.assertEquals(LispList.list(new LispInteger(5)), lispObject);
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
    LispObject lispObject = p.parseLine("'    (5 \"la la\")");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispSymbol("quote"),
        LispList.list(Arrays.<LispObject>asList(new LispInteger(5), new LispString("la la"))))), lispObject);
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
    Assert.assertNull(lispObject);
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

  @Test (expected = EndOfLineException.class)
  public void testEmptyQuote() throws LispException {
    LispObject lispObject = p.parseLine("'");
    Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispSymbol("quote"),  LispSymbol.NIL)), lispObject);
  }

  //todo
  /*@Test what expected
  public void testQuotedSpace() throws LispException {
      LispObject lispObject = p.parseLine("' ");
      Assert.assertEquals(LispList.list(Arrays.<LispObject>asList(new LispSymbol("quote"),  LispSymbol.NIL)), lispObject);
  }*/

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
  public void testParseVector() {
    LispObject lispObject = p.parseLine("[1 \"hello\" anna ['q]]");
    Assert.assertEquals(new LispVector(new LispInteger(1), new LispString("hello"), new LispSymbol("anna"), new LispVector(LispList.list(new LispSymbol("quote"), new LispSymbol("q")))), lispObject);
  }

  @Test
  public void testParseCharacter() {
    LispObject c = p.parseLine("?a");
    Assert.assertEquals(new LispInteger(97), c);
    c = p.parseLine("?A");
    Assert.assertEquals(new LispInteger(65), c);
  }

  @Test
  public void testParseZ() {
    LispObject c = p.parseLine("?\\C-\\z");
    Assert.assertEquals(new LispInteger(26), c);
    c = p.parseLine("?\\C-z");
    Assert.assertEquals(new LispInteger(26), c);
  }

  @Test
  public void testParseSpecialCharA() {
    // ?\a ⇒ 7                 ; control-g, C-g
    LispObject c = p.parseLine("?\\a");
    Assert.assertEquals(new LispInteger(7), c);
    c = p.parseLine("?\\C-g");
    Assert.assertEquals(new LispInteger(7), c);
    c = p.parseLine("?\\^g");
    Assert.assertEquals(new LispInteger(7), c);
    c = p.parseLine("?\\C-G");
    Assert.assertEquals(new LispInteger(7), c);
  }

  @Test (expected = ScanException.class)
  public void testInvalidEscapeChar() {
    p.parseLine("?\\A");
  }

  @Test
  public  void testParseCtrlA() {
    LispObject c = p.parseLine("?\\C-\\a");
//        System.out.println(((LispInteger) c).getData());
    Assert.assertEquals(new LispInteger(67108871), c);
    c = p.parseLine("?\\C-\\C-a");
//        System.out.println(((LispInteger) c).getData());
    Assert.assertEquals(new LispInteger(67108865), c);
    c = p.parseLine("?\\C-a");
//        System.out.println(((LispInteger) c).getData());
    Assert.assertEquals(new LispInteger(1), c);
    c = p.parseLine("?\\C-A");
//        System.out.println(((LispInteger) c).getData());
    Assert.assertEquals(new LispInteger(1), c);
    c = p.parseLine("?\\M-\\C-a");
    Assert.assertEquals(new LispInteger(134217729), c);
    c = p.parseLine("?\\C-\\M-a");
    Assert.assertEquals(new LispInteger(134217729), c);
  }

  @Test
  public  void testParseMetaA() {
    LispObject c = p.parseLine("?\\M-a");
    Assert.assertEquals(new LispInteger(134217825), c);
    c = p.parseLine("?\\M-\\M-a");
    Assert.assertEquals(new LispInteger(134217825), c);
    c = p.parseLine("?\\M-\\a");
    Assert.assertEquals(new LispInteger(134217735), c);
  }

  @Test
  public void testShiftA() {
    LispObject c = p.parseLine("?\\S-a");
    Assert.assertEquals(new LispInteger(33554529), c);
    c = p.parseLine("?\\S-\\a");
    Assert.assertEquals(new LispInteger(33554439), c);
    c = p.parseLine("?\\S-\\C-\\a");
    Assert.assertEquals(new LispInteger(100663303), c);
    c = p.parseLine("?\\S-\\C-a");
    Assert.assertEquals(new LispInteger(33554433), c);
  }

  @Test
  public void testHyperA() {
    LispObject c = p.parseLine("?\\H-a");
    Assert.assertEquals(new LispInteger(16777313), c);
    c = p.parseLine("?\\H-\\a");
    Assert.assertEquals(new LispInteger(16777223), c);
    c = p.parseLine("?\\H-\\C-\\a");
    Assert.assertEquals(new LispInteger(83886087), c);
    c = p.parseLine("?\\H-\\C-a");
    Assert.assertEquals(new LispInteger(16777217), c);
  }

  @Test
  public void testSuperA() {
    LispObject c = p.parseLine("?\\s-a");
    Assert.assertEquals(new LispInteger(8388705), c);
    c = p.parseLine("?\\s-\\a");
    Assert.assertEquals(new LispInteger(8388615), c);
    c = p.parseLine("?\\s-\\C-\\a");
    Assert.assertEquals(new LispInteger(75497479), c);
    c = p.parseLine("?\\s-\\C-a");
    Assert.assertEquals(new LispInteger(8388609), c);
  }

  @Test
  public void testAltA() {
    LispObject c = p.parseLine("?\\A-a");
    Assert.assertEquals(new LispInteger(4194401), c);
    c = p.parseLine("?\\A-\\a");
    Assert.assertEquals(new LispInteger(4194311), c);
    c = p.parseLine("?\\A-\\C-\\a");
    Assert.assertEquals(new LispInteger(71303175), c);
    c = p.parseLine("?\\A-\\C-a");
    Assert.assertEquals(new LispInteger(4194305), c);
  }

  @Test
  public void testAllModifiers() {
    LispObject c = p.parseLine("?\\M-\\C-\\H-\\S-\\s-\\A-a");
    Assert.assertEquals(new LispInteger(197132289), c);
    c = p.parseLine("?\\M-\\C-\\H-\\S-\\s-\\A-\\a");
    Assert.assertEquals(new LispInteger(264241159), c);
    c = p.parseLine("?\\M-\\C-\\H-\\S-\\s-\\A-z");
    Assert.assertEquals(new LispInteger(197132314), c);
    c = p.parseLine("?\\M-\\C-\\H-\\S-\\s-\\A-\\z");
    Assert.assertEquals(new LispInteger(197132314), c);
  }

  @Test
  public void testSpace() {
    LispObject c = p.parseLine("?\\ ");
    Assert.assertEquals(new LispInteger(32), c);
  }

  @Test
  public void testParseCtrl() {
    LispObject c = p.parseLine("?\\C-\\C-g");
    Assert.assertEquals(new LispInteger(67108871), c);
    c = p.parseLine("?\\^\\^g");
    Assert.assertEquals(new LispInteger(67108871), c);
    c = p.parseLine("?\\C-\\7");
    Assert.assertEquals(new LispInteger(67108871), c);
    c = p.parseLine("?\\C-\\a");
    Assert.assertEquals(new LispInteger(67108871), c);
  }

  @Test
  public void testParseCtrlNumber() {
    LispObject c = p.parseLine("?\\C-7");
    Assert.assertEquals(new LispInteger(67108919), c);
  }

  @Test
  public void testParseNumbers() {
    for (int i = 0; i < 8; ++i) {
      LispObject c = p.parseLine("?\\" + Integer.toString(i));
      Assert.assertEquals(new LispInteger(i), c);
    }
    LispObject c = p.parseLine("?\\8");
    Assert.assertEquals(new LispInteger(56), c);
    c = p.parseLine("?\\9");
    Assert.assertEquals(new LispInteger(57), c);
    c = p.parseLine("?\\10");
    Assert.assertEquals(new LispInteger(8), c);
    c = p.parseLine("?\\11");
    Assert.assertEquals(new LispInteger(9), c);
    c = p.parseLine("?\\012");
    Assert.assertEquals(new LispInteger(10), c);
    c = p.parseLine("?\\101");
    Assert.assertEquals(new LispInteger(65), c);
  }

  @Test
  public void testParseHex() {
    LispObject c = p.parseLine("?\\x41");
    Assert.assertEquals(new LispInteger(65), c);
    c = p.parseLine("?\\x1");
    Assert.assertEquals(new LispInteger(1), c);
    c = p.parseLine("?\\x8e0");
    Assert.assertEquals(new LispInteger(2272), c);
  }

  @Test
  public void testParseSpecialCharB() {
    // ?\b ⇒ 8                 ; backspace, <BS>, C-h
    LispObject c = p.parseLine("?\\b");
    Assert.assertEquals(new LispInteger(8), c);
    c = p.parseLine("?\\C-h");
    Assert.assertEquals(new LispInteger(8), c);
    c = p.parseLine("?\\^h");
    Assert.assertEquals(new LispInteger(8), c);
  }

  @Test
  public void testParseSpecialCharT() {
    //?\t ⇒ 9                 ; tab, <TAB>, C-i
    LispObject c = p.parseLine("?\\t");
    Assert.assertEquals(new LispInteger(9), c);

    c = p.parseLine("?\\C-i");
    Assert.assertEquals(new LispInteger(9), c);

    c = p.parseLine("?\\^i");
    Assert.assertEquals(new LispInteger(9), c);
  }

  @Test
  public void testParseSpecialCharN() {
    //?\n ⇒ 10                ; newline, C-j
    LispObject c = p.parseLine("?\\n");
    Assert.assertEquals(new LispInteger(10), c);
    c = p.parseLine("?\\C-j");
    Assert.assertEquals(new LispInteger(10), c);
    c = p.parseLine("?\\^j");
    Assert.assertEquals(new LispInteger(10), c);
  }

  @Test
  public void testParseSpecialCharV() {
    //?\v ⇒ 11                ; vertical tab, C-k
    LispObject c = p.parseLine("?\\v");
    Assert.assertEquals(new LispInteger(11), c);
    c = p.parseLine("?\\C-k");
    Assert.assertEquals(new LispInteger(11), c);
    c = p.parseLine("?\\^k");
    Assert.assertEquals(new LispInteger(11), c);
  }

  @Test
  public void testParseSpecialCharF() {
    //?\f ⇒ 12                ; formfeed character, C-l
    LispObject c = p.parseLine("?\\f");
    Assert.assertTrue(12 == ((LispInteger)c).getData());
    c = p.parseLine("?\\C-l");
    Assert.assertTrue(12 == ((LispInteger)c).getData());
    c = p.parseLine("?\\^l");
    Assert.assertTrue(12 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseSpecialCharR() {
    //?\r ⇒ 13                ; carriage return, <RET>, C-m
    LispObject c = p.parseLine("?\\r");
    Assert.assertTrue(13 == ((LispInteger)c).getData());
    c = p.parseLine("?\\C-m");
    Assert.assertTrue(13 == ((LispInteger)c).getData());
    c = p.parseLine("?\\^m");
    Assert.assertTrue(13 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseSpecialCharE() {
    //?\e ⇒ 27                ; escape character, <ESC>, C-[
    LispObject c = p.parseLine("?\\e");
    Assert.assertEquals(new LispInteger(27), c);
    c = p.parseLine("?\\C-\\[");
    Assert.assertEquals(new LispInteger(27), c);
  }

  @Test
  public void testParseSpecialCharS() {
    //?\s ⇒ 32                ; space character, <SPC>
    LispObject c = p.parseLine("?\\s");
    Assert.assertTrue(32 == ((LispInteger)c).getData());
  }

  @Test
  public void testParseSpecialCharSpace() {
    //?\s ⇒ 32                ; space character, <SPC>
    LispObject c = p.parseLine("? ");
    Assert.assertEquals(new LispInteger(32), c);
    c = p.parseLine("?\\ ");
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
    Assert.assertTrue(127 == ((LispInteger)c).getData());
  }

  @Ignore
  @Test
  //todo: fix Invalid modifier in string
  public void testParseSpecialCharWrong4() {
    try {
      LispObject c = p.parseLine("?\\^\"");
      System.out.println(c.toString());
    } catch (Exception e)  {
      Assert.assertEquals("\"Unbalanced parentheses\"", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testParseSpecialCharFine() {
    LispObject c = p.parseLine("?\\^\\\"");
    Assert.assertEquals(new LispInteger(67108898), c);
  }

  @Test
  public void testParseBracketNil() {
    LispObject c = p.parseLine("?\\C-\\(");
    Assert.assertEquals(new LispInteger(67108904), c);
  }

  @Ignore
  @Test
  public void testParseCharNil() {
    LispObject c = p.parseLine("?\\C-(");
    Assert.assertEquals(LispSymbol.NIL, c);
  }

  @Test
  public void testParseCharDog() {
    LispObject c = p.parseLine("?@");
    Assert.assertEquals(new LispInteger(64), c);
    c = p.parseLine("?\\A-\\C-@");
    Assert.assertEquals(new LispInteger(4194304), c);
    c = p.parseLine("?\\A-\\^@");
    Assert.assertEquals(new LispInteger(4194304), c);
  }

  //(?A . ?\A-\^@)
  @Test
  public void testParseCharCons() {
    LispObject c = p.parseLine("(?A . ?\\A-\\^@)");
    Assert.assertEquals(LispList.cons(new LispInteger(65), new LispInteger(4194304)), c);
  }

  @Test
  public void testParseChar() {
    Assert.assertEquals(new LispInteger(201326629), p.parseLine("?\\C-\\M-%"));
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

  @Test
  public void testCtrlC () {
    LispObject c = p.parseLine("?\\^c");
    Assert.assertEquals(new LispInteger(3), c);
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

//    @Test
//    public void testSymbol1() {
//        LispObject symbol = p.parseLine("\\\\a");
//        Assert.assertEquals(new LispSymbol("\\\\a"), symbol);
//    }
//
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

  @Test
  public void testParseSpace () {
    LispObject r = p.parseLine("? ");
    Assert.assertEquals(new LispInteger(32), r);
  }

  @Test
  public void testParseCtrlSpace () {
    LispObject r = p.parseLine("?\\C- ");
    Assert.assertEquals(new LispInteger(67108896), r);
  }

  @Test
  public void testParseSlashes() {
    LispObject r = p.parseLine("(insert \"\\\\C-\\\\\\\\\")");
    Assert.assertTrue(r instanceof LispList);
    LispObject cdr = ((LispList) r).cdr();
    Assert.assertTrue(cdr instanceof LispList);
    Assert.assertEquals(new LispString("\\\\C-\\\\\\\\"), ((LispList) cdr).car());
  }

  @Test
  public void testParseStringWithSlashes() {
    LispObject r = p.parseLine("\"\\\"(\\\"\"");
    Assert.assertEquals(new LispString("\"(\""), r);
  }

  @Test
  public void testParseStringWithSlashes2() {
    String s = "hello \\\"/\\\" summer \\\"\\\\\\\\\\\".";
    LispObject r = p.parseLine('"' + s + '"');
    Assert.assertEquals(new LispString(s), r);
  }

  @Test
  public void testQuoteSpaceObject() {
    LispObject r = p.parseLine("' test");
    Assert.assertEquals(LispList.list(new LispSymbol("quote"), new LispSymbol("test")), r);
  }

  @Test
  public void testCharacter() {
    LispObject list = p.parseLine("(? return)");
    Assert.assertEquals(LispList.list(new LispInteger(32), new LispSymbol("return")), list);
  }

  @Test
  public void testCharacter2() {
    LispObject list = p.parseLine("?\\700");
    Assert.assertEquals(new LispInteger(448), list);
  }

  @Test
  public void testCharacterDel() {
    LispObject list = p.parseLine("?\\C-?");
    Assert.assertEquals(new LispInteger(127), list);
  }

  @Test
  public void testCons() {
    LispObject cons = p.parseLine("(?% . %-pos)");
    Assert.assertEquals(LispList.cons(new LispInteger(37), new LispSymbol("%-pos")), cons);
  }

  @Test
  public void testConsWithSemicolon() {
    LispObject cons = p.parseLine("(?\\; . \\;-pos)");
    Assert.assertEquals(LispList.cons(new LispInteger(59), new LispSymbol("\\;-pos")), cons);
  }

  @Test
  public void testUnicodeCharacter() {
    LispObject n = p.parseLine("?\\u2014");
    Assert.assertEquals(new LispInteger(8212), n);
  }

  @Test
  public void testUnicode() {
    LispObject list = p.parseLine("(if (char-displayable-p ?\\u2014) ?\\u2014 ?-)");
    Assert.assertTrue(list instanceof LispList);

  }

  @Test
  public void testCharaters() {
    LispObject list = p.parseLine("(?&.\"&amp;\")");
    Assert.assertEquals(LispList.cons(new LispInteger(38), new LispString("&amp;")), list);
  }

  @Test
  public void testDotFloat() {
    LispObject n = p.parseLine(".5");
    Assert.assertEquals(new LispFloat(0.5), n);
  }

  @Test
  public void testDotFloatInList() {
    LispObject n = p.parseLine("(+ .5 .5)");
    Assert.assertEquals(LispList.list(new LispSymbol("+"), new LispFloat(0.5), new LispFloat(0.5)), n);
  }

  @Test
  public void test1() {
    LispObject list = p.parseLine("(insert \"  '((\\\"\\C-?\\\" . quail-delete-last-char)\n" +
        "   (\\\".\\\" . quail-next-translation)\n" +
        "   (\\\">\\\" . quail-next-translation)\n" +
        "   (\\\",\\\" . quail-prev-translation)\n" +
        "   (\\\"<\\\" . quail-prev-translation))\n" +
        "  nil nil nil nil)\\n\\n\")");
    Assert.assertTrue(list instanceof LispList);
  }

  @Test
  public void testParseFunctionSpecialFormShortcut() {
    LispObject list = p.parseLine("#'a");
    Assert.assertEquals(LispList.list(new LispSymbol("function"), new LispSymbol("a")), list);
  }

  @Test
  public void testBackSlash() {
    LispString s = (LispString) p.parseLine("\"\\\\b\"");
    Assert.assertEquals(3, s.size());
  }

  @Test
  public void testParseLineWithManyDefs() {
    LispObject o = p.parseLine("(defvar problems)       (defvar qlist)        (defvar random-adjective)");
    Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("problems")), o);
    o = p.parseNext();
    Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("qlist")), o);
    o = p.parseNext();
    Assert.assertEquals(LispList.list(new LispSymbol("defvar"), new LispSymbol("random-adjective")), o);
    Assert.assertTrue(p.isFinished());
  }

  @Test
  public void testParseStringWithTextPropNoProp() {
    Assert.assertEquals(new LispString("hello"),  p.parseLine("#(\"hello\")"));
  }

  @Test
  public void testParseStringWithTextPropOneProp() {
    LispString expected = new LispString("hello");
    expected.getTextPropertiesHolder().actOnTextProperties(1, 2, LispList.list(new LispSymbol("a"), LispSymbol.NIL), Action.ADD);
    Assert.assertEquals(expected, p.parseLine("#(\"hello\" 2 1 a)"));
  }

  @Test
  public void testParseStringWithTextPropTwoProp() {
    LispString expected = new LispString("hello");
    expected.getTextPropertiesHolder().actOnTextProperties(1, 2, LispList.list(new LispSymbol("a"), LispSymbol.NIL), Action.ADD);
    expected.getTextPropertiesHolder().actOnTextProperties(3, 5, LispList.list(new LispSymbol("q"), new LispSymbol("b"), new LispSymbol("c"), new LispSymbol("d")), Action.ADD);
    Assert.assertEquals(expected, p.parseLine("#(\"hello\" 2 1 a 3 5 (q b c d))"));
  }

  @Test
  public void testParseStringWithTextPropWrongIndex() {
    try {
      p.parseLine("#(\"hello\" 1 a a)");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument integer-or-marker-p a)", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testParseStringWithTextPropOddPropNumber() {
    try {
      p.parseLine("#(\"hello\" 1 2 (a b c))");
    } catch (Exception e) {
      Assert.assertEquals("(error \"Odd length text property list\")", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testParseIntegerBinaryRadix() {
    Assert.assertEquals(new LispInteger(-5), p.parseLine("#b-0101"));
    Assert.assertEquals(new LispInteger(-5), p.parseLine("#B-0101"));
    Assert.assertEquals(new LispInteger(-5), p.parseLine("#2r-0101"));
  }

  @Test
  public void testParseIntegerBinaryRadix2() {
    try {
      p.parseLine("#b-01015.05");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \"integer, radix 2\")", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail("#b-01015.05");
  }

  @Test
  public void testParseIntegerBinaryRadix4() {
    try {
      p.parseLine("#b+-0101");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \"integer, radix 2\")", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail("#b+-0101");
  }

  @Test
  public void testParseIntegerBinaryRadix5() {
    try {
      p.parseLine("#b2");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \"integer, radix 2\")", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail("#b2");
  }

  @Test
  public void testParseIntegerBinaryRadix3() {
    Assert.assertEquals(new LispInteger(5), p.parseLine("#b+0101.05"));
    Assert.assertEquals(new LispFloat(0.05), p.parseNext());
    Assert.assertTrue(p.isFinished());
  }

  @Test
  public void testParseOctalRadix() {
    Assert.assertEquals(new LispInteger(-110), p.parseLine("#o-156"));
    Assert.assertEquals(new LispInteger(-110), p.parseLine("#O-156"));
    Assert.assertEquals(new LispInteger(-110), p.parseLine("#8r-156"));
  }

  @Test
  public void testParseHexRadix() {
    Assert.assertEquals(new LispInteger(-87727), p.parseLine("#x-156af"));
    Assert.assertEquals(new LispInteger(-87727), p.parseLine("#X-156af"));
    Assert.assertEquals(new LispInteger(-87727), p.parseLine("#16r-156af"));
  }

  @Test
  public void testParseAnyRadix() {
    Assert.assertEquals(new LispInteger(3), p.parseLine("#3r10"));
  }

  @Test
  public void testParseMaxRadix() {
    Assert.assertEquals(new LispInteger(1331), p.parseLine("#36r10z"));
  }

  @Test
  public void testParseRadix1() {
    try {
      p.parseLine("#1r0");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \"integer, radix 1\")", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail("#1r");
  }

  @Test
  public void testParseRadix37() {
    try {
      p.parseLine("#37r123");
    } catch (Exception e) {
      Assert.assertEquals("'(invalid-read-syntax \"integer, radix 37\")", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail("#37r");
  }
}


