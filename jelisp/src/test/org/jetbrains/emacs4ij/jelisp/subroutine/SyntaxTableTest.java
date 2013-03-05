package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSyntaxTable;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/12/12
 * Time: 3:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class SyntaxTableTest extends BaseSubroutineTest {
  @Test
  public void testElispModeSyntaxTable () {
    Assert.assertNotNull(evaluateString("emacs-lisp-mode-syntax-table"));
  }

  @Test
  public void testStandardSyntaxTable () {
    LispSyntaxTable standard = SyntaxTable.getStandardSyntaxTable();
    for (char i = 0; i < 128; i++) {
      Assert.assertNotSame(LispSymbol.ourNil, standard.getCharSyntax(i));
    }
  }

  @Test
  public void testStandardSyntaxTableLength () {
    LispSyntaxTable standard = SyntaxTable.getStandardSyntaxTable();
    Assert.assertEquals(LispInteger.MAX_CHAR, standard.size());
  }
}
