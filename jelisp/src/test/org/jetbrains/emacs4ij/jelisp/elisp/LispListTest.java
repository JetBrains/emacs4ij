package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 10:28 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispListTest {

    @Test
    public void testCdr () {
        LispList list = new LispList(new LispSymbol("test"));
        LispObject list1 = list.cdr();
        Assert.assertEquals(new LispList(), list1);
    }
}
