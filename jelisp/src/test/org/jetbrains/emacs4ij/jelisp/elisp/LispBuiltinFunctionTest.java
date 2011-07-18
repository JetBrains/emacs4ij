package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.junit.Test;

import java.sql.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispBuiltinFunctionTest {

    @Test
    public void testPlus() {
        LispBuiltinFunction plus = new LispBuiltinFunction("+");
        LispObject res = plus.execute(Arrays.<LispObject>asList(new LispInteger(2), new LispInteger(3)), Environment.ourGlobal);
        Assert.assertEquals(new LispInteger(5), res);
    }

    @Test
    public void testPlusNoArgs () {
        LispBuiltinFunction plus = new LispBuiltinFunction("+");
        LispObject res = plus.execute(Arrays.<LispObject>asList(LispSymbol.ourNilSymbol), Environment.ourGlobal);
        Assert.assertEquals(new LispInteger(0), res);
    }
}
