package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import junit.framework.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

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
        Assert.assertEquals("C - X", s.toShortcutString());
    }

    @Test
    public void testToShortcutString1() throws Exception {
        LispString s = new LispString("\\C-x");
        Assert.assertEquals("ctrl X", s.toShortcutString());
    }

    @Test
    public void testToShortcutString2() throws Exception {
        LispString s = new LispString("\\C-x ");
        Assert.assertEquals("ctrl X SPACE", s.toShortcutString());
    }
    @Test
    public void testToShortcutString3() throws Exception {
        LispString s = new LispString("a");
        Assert.assertEquals("A", s.toShortcutString());
    }

    @Test
    public void testToShortcutStringTwo() throws Exception {
        LispString s = new LispString(" \\C-c\\M-a");
        Assert.assertEquals("SPACE ctrl C meta A", s.toShortcutString());
    }

    @Test
    public void testToShortcutStringManySingle() throws Exception {
        LispString s = new LispString("\\C-xf");
        Assert.assertEquals("ctrl X F", s.toShortcutString());
    }

    @Test
    public void testToShortcutStringWord() throws Exception {
        LispString s = new LispString("ivan");
        Assert.assertEquals("I V A N", s.toShortcutString());
    }

    @Test
    public void testToShortcutListManySingle() throws Exception {
        LispString s = new LispString("\\C-xf");
        List<Shortcut> list = new ArrayList<>();
        list.add(KeyboardShortcut.fromString("ctrl X"));
        list.add(KeyboardShortcut.fromString("F"));
        Assert.assertEquals(list, s.toKeyboardShortcutList());
    }
    
    @Test
    public void testToShortcutList() {
        LispString s = new LispString("\\C-a\\C-b ");
        List<Shortcut> list = new ArrayList<>();
        list.add(KeyboardShortcut.fromString("ctrl A"));
        list.add(KeyboardShortcut.fromString("ctrl B"));
        list.add(KeyboardShortcut.fromString("SPACE"));
        Assert.assertEquals(list, s.toKeyboardShortcutList());
    }
}
