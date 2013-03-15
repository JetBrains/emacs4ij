package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.text.Action;
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
    a.getTextPropertiesHolder().actOnTextProperties(0, 1, LispList.list(new LispSymbol("q")), Action.ADD);
    LispString b = new LispString("a");
    Assert.assertFalse(a.getTextPropertiesHolder().noTextProperties());
    Assert.assertTrue(b.getTextPropertiesHolder().noTextProperties());
    Assert.assertFalse(a.equals(b));
  }
}
