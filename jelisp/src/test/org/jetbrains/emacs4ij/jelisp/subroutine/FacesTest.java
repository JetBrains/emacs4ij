package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.util.XlfdField;
import org.junit.Test;

import java.util.Arrays;

import static junit.framework.Assert.assertEquals;

public class FacesTest extends BaseSubroutineTest {
  @Test(expected = WrongTypeArgumentException.class)
  public void internalSetFontSelectionOrder1() {
    evaluateString("(internal-set-font-selection-order \"a\")");
  }

  @Test(expected = LispException.class)
  public void internalSetFontSelectionOrder2() {
    evaluateString("(internal-set-font-selection-order '(1 2))");
  }

  @Test(expected = LispException.class)
  public void internalSetFontSelectionOrder3() {
    evaluateString("(internal-set-font-selection-order '(:height))");
  }

  @Test
  public void internalSetFontSelectionOrder4() {
    assertEquals(Arrays.asList(XlfdField.SET_WIDTH, XlfdField.POINT_SIZE, XlfdField.WEIGHT, XlfdField.SLANT), Faces.getFontSortOrder());
    assertEquals(LispSymbol.ourNil, evaluateString("(internal-set-font-selection-order '(:height :weight :width :slant))"));
    assertEquals(Arrays.asList(XlfdField.POINT_SIZE, XlfdField.WEIGHT, XlfdField.SET_WIDTH, XlfdField.SLANT), Faces.getFontSortOrder());
  }
}
