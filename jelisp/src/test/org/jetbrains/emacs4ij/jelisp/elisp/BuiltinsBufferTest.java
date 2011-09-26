package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;
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
        LispObject lispObject = BuiltinsBuffer.bufferSize(environment, p(LispSymbol.ourNil));
        Assert.assertEquals(new LispInteger(0), lispObject);
    }

    @Test (expected = EnvironmentException.class)
    public void testCurrentBuffer () {
        BuiltinsBuffer.getCurrentBuffer(environment);
    }

    @Test (expected = WrongTypeArgument.class)
    public void testSetBufferWrongType() {
        BuiltinsBuffer.setBuffer(environment, p(new LispInteger(5)));
    }

    @Test (expected = NoBufferException.class)
    public void testSetBufferNoBuffer() {
        BuiltinsBuffer.setBuffer(environment, p(new LispString("hello.lisp")));
    }

}
