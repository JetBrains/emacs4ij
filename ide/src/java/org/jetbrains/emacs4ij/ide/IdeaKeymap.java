package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.keymap.KeymapManagerListener;
import com.intellij.openapi.keymap.KeymapUtil;
import com.intellij.openapi.keymap.impl.DefaultKeymap;
import com.intellij.openapi.keymap.impl.KeymapImpl;
import com.intellij.openapi.keymap.impl.KeymapManagerImpl;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ArrayUtil;
import com.rits.cloning.Cloner;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.LogUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.StringOrVector;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import javax.swing.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class IdeaKeymap implements LispKeymap {
  private final String myName;
  private LispKeymap myParent = null;
  private Map<Shortcut, LispObject> myKeyBindings = new HashMap<>();
  private static final KeymapManagerImpl ourKeymapManager = (KeymapManagerImpl) KeymapManager.getInstance();
  private static final KeymapImpl myIdeaKeymap;

  static {
    KeymapImpl tmp;
    try {
      KeymapImpl emacsKeymap = (KeymapImpl) getIdeaEmacsKeyMap();
      emacsKeymap.setCanModify(false);
      tmp = emacsKeymap.deriveKeymap();
      tmp.setName(Emacs4ijBundle.message("emacs4ij"));
      tmp.setCanModify(true);
    } catch (NullPointerException e) {
      tmp = null;
    }
    myIdeaKeymap = tmp;
    ourKeymapManager.addKeymap(myIdeaKeymap);

    ourKeymapManager.addKeymapManagerListener(new KeymapManagerListener() {
      @Override
      public void activeKeymapChanged(Keymap keymap) {
        LogUtil.info("Keymap: incoming: " + keymap.getName() + " = " + keymap.toString()
            + ", current: " + ourKeymapManager.getActiveKeymap().getName() + " = " + ourKeymapManager.getActiveKeymap().toString());

        //if user changes keymap to non-emacs?
        //todo: remove my bindings from old keymap. Should I add them to the new one?
        // or should i derive myIdeaKeymap from new one?
      }
    });
    ourKeymapManager.setActiveKeymap(myIdeaKeymap);
  }

  private static Keymap getIdeaEmacsKeyMap () {
    for (Keymap keymap: DefaultKeymap.getInstance().getKeymaps()) {
      if (KeymapUtil.isEmacsKeymap(keymap)) return keymap;
    }
    throw new IllegalStateException();
  }

  public IdeaKeymap(@Nullable LispObject name, @Nullable LispKeymap parent) {
    myParent = parent;
    myName = name == null
        ? Emacs4ijBundle.message("empty.keymap.name")
        : name instanceof LispString ? ((LispString) name).getData() : name.toString();
  }

  @Override
  public void defineKey(LispObject action, StringOrVector key) {
    Shortcut shortcut = defineKey(action, key.toKeyboardShortcutList(), 0);
    if (shortcut == null) {
      GlobalEnvironment.echo("Couldn't assign " + key + " to " + action + " in keymap " + this, GlobalEnvironment.MessageType.WARNING);
      return;
    }
    if (GlobalEnvironment.INSTANCE.isKeymapActive(this))
      registerAction(action, shortcut);
  }

  private Shortcut defineKey(LispObject action, @Nullable List<Shortcut> shortcuts, int index) {
    if (shortcuts == null || shortcuts.isEmpty())
      return null;
    Shortcut firstKeystroke = shortcuts.get(index);
    if (shortcuts.size() - 1 == index) {
      //while i don't support multiple shortcuts, save max 2:
      Shortcut shortcut = shortcuts.get(0);
      if (shortcuts.size() > 1 && shortcuts.get(0) instanceof KeyboardShortcut && shortcuts.get(1) instanceof KeyboardShortcut) {
        shortcut = new KeyboardShortcut(((KeyboardShortcut) shortcuts.get(0)).getFirstKeyStroke(),
            ((KeyboardShortcut) shortcuts.get(1)).getFirstKeyStroke());
      }
      if (org.jetbrains.emacs4ij.jelisp.subroutine.Predicate.isNil(action))
        myKeyBindings.remove(firstKeystroke);
      else
        myKeyBindings.put(firstKeystroke, action);
      return shortcut;
    }
    IdeaKeymap prefix = getPrefixKeymap(firstKeystroke);
    if (prefix != null) {
      return prefix.defineKey(action, shortcuts, index + 1);
    }
    prefix = new IdeaKeymap(null, this);
    defineKey(prefix, Arrays.asList(firstKeystroke), 0);
    return prefix.defineKey(action, shortcuts, index + 1);
//        Core.error(Emacs4ijBundle.message("non.prefix.first.keystroke", shortcuts.toString(), firstKeystroke.toString()));
//        return null;
  }

  private IdeaKeymap getPrefixKeymap (Shortcut shortcut) {
    if (!myKeyBindings.containsKey(shortcut))
      return null;
    return getKeymap(myKeyBindings.get(shortcut));
  }

  private String generateActionId (String binding) {
    return (Emacs4ijBundle.message("emacs4ij") + StringUtil.capitalizeWords(binding, "-", true, false)).replaceAll(" ", "");
  }

  private void unregisterShortcut (Shortcut shortcut) {
    String[] actionIds = myIdeaKeymap.getActionIds(shortcut);
    while (actionIds != ArrayUtil.EMPTY_STRING_ARRAY) {
      for (String actionId: actionIds) {
        myIdeaKeymap.removeShortcut(actionId, shortcut);
      }
      actionIds = myIdeaKeymap.getActionIds(shortcut);
    }
  }

  private Map<Shortcut, LispObject> getBindings() {
    return myKeyBindings;
  }

  @Nullable
  private IdeaKeymap getKeymap (LispObject action) {
    if (action instanceof KeymapCell)
      return (IdeaKeymap)((KeymapCell) action).getKeymap();
    return null;
  }

  private void registerAction (LispObject action, Shortcut shortcut) {
    if (!(shortcut instanceof KeyboardShortcut))
      return;
    IdeaKeymap inner = getKeymap(action);
    if (inner == null) {
      registerKbdAction(action, (KeyboardShortcut) shortcut);
      return;
    }
    //only 2 keystrokes are supported
    if (((KeyboardShortcut) shortcut).getSecondKeyStroke() != null)
      return;
    KeyStroke first = ((KeyboardShortcut) shortcut).getFirstKeyStroke();
    for (Map.Entry<Shortcut, LispObject> entry: inner.getBindings().entrySet()) {
      if (!(entry.getKey() instanceof KeyboardShortcut)
          || (entry.getKey() instanceof KeyboardShortcut && ((KeyboardShortcut) entry.getKey()).getSecondKeyStroke() != null)
          || getKeymap(entry.getValue()) != null)
        continue;
      KeyStroke second = ((KeyboardShortcut) entry.getKey()).getFirstKeyStroke();
      registerKbdAction(entry.getValue(), new KeyboardShortcut(first, second));
    }
  }

  private void registerKbdAction(LispObject action, KeyboardShortcut shortcut) {
    ActionManager actionManager = ActionManager.getInstance();
    String id = generateActionId(action.toString());
    if (actionManager.getActionIds(id).length == 0)
      actionManager.registerAction(id, new EmacsAction(action), PluginId.getId(Emacs4ijBundle.message("emacs4ij")));
    unregisterShortcut(shortcut);
    myIdeaKeymap.addShortcut(id, shortcut);
  }

  private Stack<LispKeymap> getKeymapStack() {
    Stack<LispKeymap> stack = new Stack<>();
    stack.push(this);
    for (LispKeymap parent = myParent; parent != null; parent = parent.getParent())
      stack.push(parent);
    return stack;
  }

  @Override
  public void bindActions (@Nullable LispKeymap current) {
    if (current != null && myParent == current) {
      bind();
      return;
    }
    myIdeaKeymap.clearOwnActionsIds();
    Stack<LispKeymap> keymaps = getKeymapStack();
    while (!keymaps.isEmpty()) {
      ((IdeaKeymap)keymaps.pop()).bind();
    }
  }

  protected void bind() {
    for (Map.Entry<Shortcut, LispObject> entry: myKeyBindings.entrySet()) {
      registerAction(entry.getValue(), entry.getKey());
    }
  }

  @Override
  public LispKeymap getParent() {
    return myParent;
  }

  private boolean equalsOrIsParentFor (LispKeymap keymap) {
    for (LispKeymap parent = keymap; parent != null; parent = parent.getParent())
      if (this == parent)
        return true;
    return false;
  }

  @Override
  public void setParent(@Nullable LispKeymap parent) {
    if (equalsOrIsParentFor(parent))
      Core.error("Cyclic keymap inheritance");
    myParent = parent;
    for (LispObject cell: myKeyBindings.values()) {
      if (cell instanceof LispKeymap)
        ((LispKeymap) cell).setParent(parent);
    }
  }

  @Override
  public LispKeymap copy() {
    return new Cloner().deepClone(this);
  }

  @Override
  public boolean isEmpty() {
    return myKeyBindings.isEmpty();
  }

  @Override
  public String getName() {
    return myName;
  }

  @Override
  public LispObject getKeyBinding(LispObject key) {
    //todo: accept LispInteger
    LispObject binding = null;
    if (key instanceof StringOrVector)
      binding = getKeyBinding(((StringOrVector)key).toKeyboardShortcutList());
    return Core.thisOrNil(binding);
  }

  private LispObject getKeyBinding(@Nullable List<Shortcut> shortcuts) {
    if (shortcuts == null || isEmpty() || shortcuts.isEmpty())
      return null;
    Shortcut firstKeystroke = shortcuts.get(0);
    if (shortcuts.size() == 1)
      return getKeyBinding(firstKeystroke);
    LispKeymap prefix = getPrefixKeymap(firstKeystroke);
    if (prefix != null) {
      return ((IdeaKeymap)prefix).getKeyBinding(shortcuts.subList(1, shortcuts.size()));
    }
    return null;
  }

  private LispObject getKeyBinding (Shortcut shortcut) {
    LispObject function = myKeyBindings.get(shortcut);
    if (function != null)
      return function;
    if (myParent != null)
      return ((IdeaKeymap)myParent).getKeyBinding(shortcut);
    return LispSymbol.NIL;
    //get idea binding

//        String[] actions = ourKeymapManager.getActiveKeymap().getActionIds(shortcut);
//        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
//            return null;
//        if (actions.length != 1)
//            throw new Attention();
//        return new LispSymbol(actions[0]);
  }

  @Override
  public String toString() {
    return myName;
  }

  @Override
  public LispObject evaluate(Environment environment) {
    return this;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof IdeaKeymap)) return false;

    IdeaKeymap that = (IdeaKeymap) o;

    if (myKeyBindings != null ? !myKeyBindings.equals(that.myKeyBindings) : that.myKeyBindings != null)
      return false;
    if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;
    if (myParent != null ? !myParent.equals(that.myParent) : that.myParent != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = myName != null ? myName.hashCode() : 0;
    result = 31 * result + (myParent != null ? myParent.hashCode() : 0);
    result = 31 * result + (myKeyBindings != null ? myKeyBindings.hashCode() : 0);
    return result;
  }

  @Override
  public LispKeymap getKeymap() {
    return this;
  }
}
