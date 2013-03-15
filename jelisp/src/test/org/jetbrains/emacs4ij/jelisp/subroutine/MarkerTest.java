package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Test;

public class MarkerTest extends JelispTestCase {
  @Test
  public void testMarkerInsertionType() throws Exception {
    LispMarker marker = new LispMarker();
    Assert.assertEquals(LispSymbol.NIL, Marker.markerInsertionType(marker));
  }

  @Test
  public void testMakeMarker () {
    LispObject marker = evaluateString("(make-marker)");
    Assert.assertEquals(new LispMarker(), marker);
  }

  @Test
  public void testSetMarkerInsertionType() throws Exception {
    evaluateString("(defvar m (make-marker))");
    LispObject lispObject =  evaluateString("(set-marker-insertion-type m ())");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(marker-insertion-type m)"));
    Assert.assertEquals(LispSymbol.NIL, lispObject);

    lispObject =  evaluateString("(set-marker-insertion-type m (+ 5 5))");
    Assert.assertEquals(LispSymbol.T, evaluateString("(marker-insertion-type m)"));
    Assert.assertEquals(new LispInteger(10), lispObject);
  }
}
