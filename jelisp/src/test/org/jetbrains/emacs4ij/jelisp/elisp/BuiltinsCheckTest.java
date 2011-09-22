package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/18/11
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsCheckTest {

    private Environment environment;

    @Before
    public void setUp() {
        environment = new Environment(new Environment());
    }

    private List<LispObject> p(LispObject... objects) {
        return Arrays.asList(objects);
    }

    @Test
    public void testStringp() throws Exception {
        LispObject lispObject = BuiltinsCheck.stringp(environment, p(new LispString("hello")));
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = BuiltinsCheck.stringp(environment, p(new LispSymbol("hello")));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSymbolp() throws Exception {
        LispObject lispObject = BuiltinsCheck.symbolp(environment, p(new LispString("hello")));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = BuiltinsCheck.symbolp(environment, p(new LispSymbol("hello")));
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testIntegerp() throws Exception {
        LispObject lispObject = BuiltinsCheck.integerp(environment, p(new LispInteger(1)));
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = BuiltinsCheck.integerp(environment, p(new LispSymbol("hello")));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSubrp() throws Exception {
        //todo: implement
    }

}
