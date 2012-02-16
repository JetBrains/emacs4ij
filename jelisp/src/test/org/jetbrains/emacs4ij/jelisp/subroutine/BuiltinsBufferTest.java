package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsBufferTest {
    private CustomEnvironment environment;

    @BeforeClass
    public static void runBeforeClass() {
        TestSetup.runBeforeClass();
    }

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.INSTANCE.clearRecorded();
        environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
    }

  /*  private List<LObject> p(LObject... objects) {
        return Arrays.asList(objects);
    }            */


    @Test (expected = NoOpenedBufferException.class)
    public void testCurrentBuffer () {
        BuiltinsBuffer.getCurrentBuffer(environment);
    }

    @Test (expected = NoBufferException.class)
    public void testSetBufferNoBuffer() {
        BuiltinsBuffer.setBuffer(environment, new LispString("hello.lisp"));
    }

}
