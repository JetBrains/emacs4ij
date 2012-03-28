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
