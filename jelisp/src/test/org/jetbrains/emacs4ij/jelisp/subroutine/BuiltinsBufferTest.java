package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsBufferTest extends BaseSubroutineTest {
    @Test (expected = NoOpenedBufferException.class)
    public void testCurrentBuffer () {
        BuiltinsBuffer.getCurrentBuffer(myEnvironment);
    }

    @Test (expected = NoBufferException.class)
    public void testSetBufferNoBuffer() {
        BuiltinsBuffer.setBuffer(myEnvironment, new LispString("hello.lisp"));
    }

}
