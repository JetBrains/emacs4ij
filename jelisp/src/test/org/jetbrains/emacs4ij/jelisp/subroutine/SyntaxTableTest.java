package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSyntaxTable;
import org.junit.Assert;
import org.junit.Test;

public class SyntaxTableTest extends JelispTestCase {
  @Test
  public void testElispModeSyntaxTable () {
    Assert.assertNotNull(evaluateString("emacs-lisp-mode-syntax-table"));
  }

  @Test
  public void testStandardSyntaxTable () {
    LispSyntaxTable standard = SyntaxTable.getStandardSyntaxTable();
    for (char i = 0; i < 128; i++) {
      Assert.assertNotSame(LispSymbol.NIL, standard.getCharSyntax(i));
    }
  }

  @Test
  public void testStandardSyntaxTableLength () {
    LispSyntaxTable standard = SyntaxTable.getStandardSyntaxTable();
    Assert.assertEquals(LispInteger.MAX_CHAR, standard.size());
  }
}
