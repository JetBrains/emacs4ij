package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.junit.Test;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/15/12
 * Time: 12:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispStringTest {
    @Test
    public void testToShortcutString() throws Exception {
        LispString s = new LispString("C-x");
        Assert.assertEquals("ctrl X", s.toShortcutString());
    }
    @Test
    public void testToShortcutString1() throws Exception {
        LispString s = new LispString("\\C-x");
        Assert.assertEquals("ctrl X", s.toShortcutString());
    }

    @Test
    public void testToShortcutString2() throws Exception {
        LispString s = new LispString("C-x ");
        Assert.assertEquals("ctrl X ", s.toShortcutString());
    }
    @Test
    public void testToShortcutString3() throws Exception {
        LispString s = new LispString("a");
        Assert.assertEquals("A", s.toShortcutString());
    }
}
