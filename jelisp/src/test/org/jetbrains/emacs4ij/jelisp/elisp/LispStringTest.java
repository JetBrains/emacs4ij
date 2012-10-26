package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import junit.framework.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class LispStringTest {
    @Test
    public void testToShortcutListManySingle() {
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

    @Test
    public void testStringEquality() {
        LispString a = new LispString("a");
        a.actOnTextProperties(0, 1, LispList.list(new LispSymbol("q")), TextPropertiesHolder.Action.ADD);
        LispString b = new LispString("a");
        Assert.assertFalse(a.noTextProperties());
        Assert.assertTrue(b.noTextProperties());
        Assert.assertFalse(a.equals(b));
    }
}
