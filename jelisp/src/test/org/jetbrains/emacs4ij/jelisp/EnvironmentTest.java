package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuiltinFunction;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class EnvironmentTest {

    @Test
    public void testGetBuiltInF () {
        Environment e = Environment.ourGlobal;
        LispObject lispObject = e.find("+", Environment.SymbolType.FUNCTION);
        Assert.assertTrue(lispObject instanceof LispBuiltinFunction);
        Assert.assertEquals(new LispSymbol("+"), ((LispBuiltinFunction) lispObject).getName());
    }

    @Test
    public void testOverrideVar () {
        Environment e = new Environment(Environment.ourGlobal);
        e.setVariable(new LispSymbol("a"), new LispInteger(5));
        e.setVariable(new LispSymbol("a"), new LispInteger(6));
        Assert.assertEquals(new LispInteger(6), e.getVariable("a"));
    }

}
