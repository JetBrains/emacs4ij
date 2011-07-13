package org.jetbrains.emacs4ij.jelisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuiltinFunction;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
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
        Environment e = new Environment(null);
        LispObject lispObject = e.find("+");
        Assert.assertTrue(lispObject instanceof LispBuiltinFunction);
        Assert.assertEquals("+", ((LispBuiltinFunction) lispObject).getName());
    }
}
