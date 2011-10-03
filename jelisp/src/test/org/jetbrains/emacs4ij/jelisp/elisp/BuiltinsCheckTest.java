package org.jetbrains.emacs4ij.jelisp.elisp;

import org.junit.Assert;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/18/11
 * Time: 3:55 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsCheckTest {

     @Test
    public void testStringp() throws Exception {
        LispObject lispObject = BuiltinsCheck.stringp(new LispString("hello"));
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = BuiltinsCheck.stringp(new LispSymbol("hello"));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSymbolp() throws Exception {
        LispObject lispObject = BuiltinsCheck.symbolp(new LispString("hello"));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = BuiltinsCheck.symbolp(new LispSymbol("hello"));
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testIntegerp() throws Exception {
        LispObject lispObject = BuiltinsCheck.integerp(new LispInteger(1));
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = BuiltinsCheck.integerp(new LispSymbol("hello"));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testSubrp() throws Exception {
        //TODO: non nil

        LispObject lispObject = BuiltinsCheck.subrp(new LispInteger(1));
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

}
