package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/3/11
 * Time: 5:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class MarkerTest extends BaseSubroutineTest {
    @Test
    public void testMarkerInsertionType() throws Exception {
        LispMarker marker = new LispMarker();
        Assert.assertEquals(LispSymbol.ourNil, Marker.markerInsertionType(marker));
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
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(marker-insertion-type m)"));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);

        lispObject =  evaluateString("(set-marker-insertion-type m (+ 5 5))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(marker-insertion-type m)"));
        Assert.assertEquals(new LispInteger(10), lispObject);
    }
}
