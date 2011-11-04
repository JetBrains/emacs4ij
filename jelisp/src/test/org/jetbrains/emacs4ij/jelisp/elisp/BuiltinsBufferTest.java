package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:52 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsBufferTest {
    private Environment environment;

    @Before
    public void setUp() {
        environment = new Environment(new Environment());
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
