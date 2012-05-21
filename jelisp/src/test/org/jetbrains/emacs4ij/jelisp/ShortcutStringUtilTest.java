package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.junit.Test;

import javax.swing.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/28/12
 * Time: 1:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class ShortcutStringUtilTest {
    @Test
    public void testToShortcutString() throws Exception {
        LispString s = new LispString("C-x");
        Assert.assertEquals("C MINUS X", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutString1() throws Exception {
        LispString s = new LispString("\\C-x");
        Assert.assertEquals("ctrl X", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutString2() throws Exception {
        LispString s = new LispString("\\C-x ");
        Assert.assertEquals("ctrl X SPACE", ShortcutStringUtil.toShortcutString(s));
    }
    @Test
    public void testToShortcutString3() throws Exception {
        LispString s = new LispString("a");
        Assert.assertEquals("A", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutStringTwo() throws Exception {
        LispString s = new LispString(" \\C-c\\M-a");
        Assert.assertEquals("SPACE ctrl C alt A", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutStringManySingle() throws Exception {
        LispString s = new LispString("\\C-xf");
        Assert.assertEquals("ctrl X F", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutStringWord() throws Exception {
        LispString s = new LispString("ivan");
        Assert.assertEquals("I V A N", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutStringF() throws Exception {
        LispString s = new LispString("<f1><f12>");
        Assert.assertEquals("F1 F12", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutStringSafety() throws Exception {
        LispString s = new LispString("F1 F2");
        Assert.assertEquals("F 1 \\SPACE F 2", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testToShortcutsSafety() throws Exception {
        LispString s = new LispString("F1 F2");
        List<Shortcut> list = new ArrayList<>();
        list.add(KeyboardShortcut.fromString("F"));
        list.add(KeyboardShortcut.fromString("1"));
        list.add(KeyboardShortcut.fromString("SPACE"));
        list.add(KeyboardShortcut.fromString("F"));
        list.add(KeyboardShortcut.fromString("2"));
        Assert.assertEquals(list, ShortcutStringUtil.toKeyboardShortcutList(s));
    }

    @Test
    public void testQuestionToShortcutString() {
        LispString s = new LispString("?");
        Assert.assertEquals("shift SLASH", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testSlashEToShortcutString() {
        LispString s = new LispString("\\e");
        Assert.assertEquals("ESCAPE", ShortcutStringUtil.toShortcutString(s));
    }

    @Test
    public void testQuestionToShortcutList() {
        LispString s = new LispString("?");
        List<Shortcut> list = new ArrayList<>();
        list.add(KeyboardShortcut.fromString("shift SLASH"));
        Assert.assertEquals(list, ShortcutStringUtil.toKeyboardShortcutList(s));
    }

    @Test
    public void testSlashedNumber() {
        Assert.assertEquals("127", ShortcutStringUtil.toShortcutString(new LispString("\\177")));
    }

    @Test
    public void testToShortcutsSlashedNumber() throws Exception {
        LispString s = new LispString("\\177");
        List<Shortcut> list = new ArrayList<>();
        list.add(new KeyboardShortcut(KeyStroke.getKeyStroke((char)127), null));
        Assert.assertEquals(list, ShortcutStringUtil.toKeyboardShortcutList(s));
    }
}
