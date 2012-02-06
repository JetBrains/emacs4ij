package org.jetbrains.emacs4ij.jelisp.subroutine;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/6/12
 * Time: 5:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsSequenceTest {
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
    public void testLength() throws Exception {
        LObject r = evaluateString("(length '(1 2 3))");
        Assert.assertEquals(new LispInteger(3), r);
        r = evaluateString("(length ())");
        Assert.assertEquals(new LispInteger(0), r);
        r = evaluateString("(length \"foobar\")");
        Assert.assertEquals(new LispInteger(6), r);
        r = evaluateString("(length [1 2 3])");
        Assert.assertEquals(new LispInteger(3), r);
        //todo: (length (make-bool-vector 5 nil)) ⇒ 5
    }
}
