package org.jetbrains.emacs4ij.jelisp.elisp;

import org.junit.Assert;
import org.junit.Test;

public class SyntaxDescriptorTest {
  @Test
  public void testStringSyntax() {
    Assert.assertEquals(LispList.list(new LispInteger(0)), SyntaxDescriptor.toSyntaxTableEntry("-"));
  }

  @Test
  public void testStringSyntax0() {
    Assert.assertEquals(LispList.list(new LispInteger(0)), SyntaxDescriptor.toSyntaxTableEntry(" "));
  }

  @Test
  public void testStringSyntax1() {
    Assert.assertEquals(LispList.list(new LispInteger(1)), SyntaxDescriptor.toSyntaxTableEntry(". "));
  }

  @Test
  public void testStringSyntax2() {
    Assert.assertEquals(LispList.list(new LispInteger(65537)), SyntaxDescriptor.toSyntaxTableEntry(". 1"));
  }

  @Test
  public void testStringSyntax3() {
    Assert.assertEquals(LispList.list(new LispInteger(4784129)), SyntaxDescriptor.toSyntaxTableEntry(". 14n"));
  }

  @Test
  public void testStringSyntaxList() {
    Assert.assertEquals(LispList.cons(new LispInteger(5), new LispInteger(36)),
        SyntaxDescriptor.toSyntaxTableEntry(")$"));
  }

  @Test
  public void testLongStringSyntaxList() {
    Assert.assertEquals(LispList.cons(new LispInteger(524288), new LispInteger(41)),
        SyntaxDescriptor.toSyntaxTableEntry(" )$45"));
  }

  @Test
  public void testLongStringSyntaxList2() {
    Assert.assertEquals(LispList.cons(new LispInteger(524288), new LispInteger(41)),
        SyntaxDescriptor.toSyntaxTableEntry(" )$45. "));
  }

  @Test
  public void testLongStringSyntaxInherit() {
    Assert.assertEquals(LispList.list(), SyntaxDescriptor.toSyntaxTableEntry("@ )$45. "));
  }

  @Test
  public void testStringSyntaxInherit() {
    Assert.assertEquals(LispList.list(), SyntaxDescriptor.toSyntaxTableEntry("@"));
  }

  @Test
  public void testGetSyntaxClassWhitespace() {
    Assert.assertEquals(SyntaxDescriptor.ClassType.WHITESPACE, SyntaxDescriptor.classBySyntaxCode(524288));
  }

  @Test
  public void testGetSyntaxClassWhitespace2() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("- 123");
    Assert.assertEquals(SyntaxDescriptor.ClassType.WHITESPACE, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassPunctuation() {
    int code = SyntaxDescriptor.makeFullSyntaxCode(". 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.PUNCTUATION, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassWord() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("w 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.WORD, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassSymbol() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("_ 123pbn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.SYMBOL, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassOpenP() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("( 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.OPEN_PARENTHESIS, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassCloseP() {
    int code = SyntaxDescriptor.makeFullSyntaxCode(") 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.CLOSE_PARENTHESIS, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassExpression() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("\' 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.EXPRESSION_PREFIX, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassString() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("\" 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.STRING_QUOTE, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassPairedDelimiter() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("$ 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.PAIRED_DELIMITER, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassEscape() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("\\ 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.ESCAPE, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassCharQuote() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("/ 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.CHARACTER_QUOTE, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassCommentStart() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("< 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.COMMENT_START, SyntaxDescriptor.classBySyntaxCode(code));
  }
  @Test
  public void testGetSyntaxClassCommentEnd() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("> 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.COMMENT_END, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassGenericComment() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("! 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.GENERIC_COMMENT_DELIMITER, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testGetSyntaxClassGenericString() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("| 123bn");
    Assert.assertEquals(SyntaxDescriptor.ClassType.GENERIC_STRING_DELIMITER, SyntaxDescriptor.classBySyntaxCode(code));
  }

  @Test
  public void testIsStart2CharCommentStart() {
    int code = SyntaxDescriptor.makeFullSyntaxCode("| 123bn");
    Assert.assertTrue(SyntaxDescriptor.is(SyntaxDescriptor.FlagType.START_2CHAR_COMMENT_START, code));
    code = SyntaxDescriptor.makeFullSyntaxCode("| 23bn");
    Assert.assertFalse(SyntaxDescriptor.is(SyntaxDescriptor.FlagType.START_2CHAR_COMMENT_START, code));
  }
}
