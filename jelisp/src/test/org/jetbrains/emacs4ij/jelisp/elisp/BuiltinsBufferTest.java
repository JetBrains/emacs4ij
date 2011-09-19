package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.EnvironmentException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

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

    private List<LObject> p(LObject... objects) {
        return Arrays.asList(objects);
    }

    @Test  (expected = EnvironmentException.class)
    public void testBufferSize() {
       // LispBuffer b = new LispBuffer("buffer1");
        LispObject lispObject = BuiltinsBuffer.bufferSize(environment, p(LispSymbol.ourNil));
        Assert.assertEquals(new LispInteger(0), lispObject);
    }

    @Test (expected = EnvironmentException.class)
    public void testCurrentBuffer () {
        LispBuffer currentBuffer = BuiltinsBuffer.getCurrentBuffer(environment, null);
        Assert.assertEquals("*scratch*", currentBuffer.getName());
    }
}
