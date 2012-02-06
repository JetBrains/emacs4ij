package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/3/11
 * Time: 5:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsMarkerTest {
    private CustomEnvironment environment;

    @BeforeClass
    public static void runBeforeClass() {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        GlobalEnvironment.initialize(null, null);
        GlobalEnvironment.INSTANCE.startRecording();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

    private LObject evaluateString (String lispCode) throws LispException {
        Parser parser = new Parser();
        return parser.parseLine(lispCode).evaluate(environment);
    }

    @Test
    public void testMarkerInsertionType() throws Exception {
        LispMarker marker = new LispMarker(10, null);
        Assert.assertEquals(LispSymbol.ourNil, BuiltinsMarker.markerInsertionType(marker));
    }

    @Test
    public void testMakeMarker () {
        LObject marker = evaluateString("(make-marker)");
        Assert.assertEquals(new LispMarker(), marker);
    }

    @Test
    public void testSetMarkerInsertionType() throws Exception {
        evaluateString("(defvar m (make-marker))");
        LObject lispObject =  evaluateString("(set-marker-insertion-type m ())");
        Assert.assertEquals(LispSymbol.ourNil, evaluateString("(marker-insertion-type m)"));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);

        lispObject =  evaluateString("(set-marker-insertion-type m (+ 5 5))");
        Assert.assertEquals(LispSymbol.ourT, evaluateString("(marker-insertion-type m)"));
        Assert.assertEquals(new LispInteger(10), lispObject);
    }
}
