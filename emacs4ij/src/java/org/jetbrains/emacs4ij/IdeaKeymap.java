package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.keymap.KeymapUtil;
import com.intellij.openapi.keymap.impl.DefaultKeymap;
import com.intellij.openapi.keymap.impl.KeymapImpl;
import com.intellij.openapi.keymap.impl.KeymapManagerImpl;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ArrayUtil;
import com.rits.cloning.Cloner;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import javax.swing.*;
import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 3:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaKeymap implements LispKeymap {
    private final String myName;
    private LispKeymap myParent = null;
    private Map<Shortcut, KeymapCell> myKeyBindings = new HashMap<>();
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

//        ourKeymapManager.addKeymapManagerListener(new KeymapManagerListener() {
//            @Override
//            public void activeKeymapChanged(Keymap keymap) {
////                System.out.println("incoming: " + keymap.getName() + " = " + keymap.toString() +
////                        ", current: " + ourKeymapManager.getActiveKeymap().getName() + " = " + ourKeymapManager.getActiveKeymap().toString());
//                //if user changes keymap to non-emacs?
//                //todo: remove my bindings from old keymap. Should I add them to the new one?
//                // or should i derive myIdeaKeymap from new one?
//            }
//        });
        ourKeymapManager.setActiveKeymap(myIdeaKeymap);
    }
    
    private static Keymap getIdeaEmacsKeyMap () {
        return (Keymap) CollectionUtils.find(
                Arrays.asList(DefaultKeymap.getInstance().getKeymaps()),
                new Predicate() {
                    @Override
                    public boolean evaluate(Object o) {
                        return KeymapUtil.isEmacsKeymap((Keymap) o);
                    }
                });
    }

    public IdeaKeymap(@Nullable LispObject name, @Nullable LispKeymap parent) {
        myParent = parent;
        myName = name == null
                ? Emacs4ijBundle.message("empty.keymap.name")
                : name instanceof LispString ? ((LispString) name).getData() : name.toString();
    }

    @Override
    public void defineKey(KeymapCell action, StringOrVector key) {
        Shortcut shortcut = defineKey(action, key.toKeyboardShortcutList(), 0);
        if (GlobalEnvironment.INSTANCE.isKeymapActive(this) && shortcut != null)
            registerAction(action, shortcut);
    }

    private Shortcut defineKey(KeymapCell action, List<Shortcut> shortcuts, int index) {
        if (shortcuts.isEmpty())
            return null;
        Shortcut firstKeystroke = shortcuts.get(index);
        if (shortcuts.size() - 1 == index) {
            //while i don't support multiple shortcuts, save max 2:
            Shortcut shortcut = shortcuts.get(0);
            if (shortcuts.size() > 1 && shortcuts.get(0) instanceof KeyboardShortcut && shortcuts.get(1) instanceof KeyboardShortcut) {
                shortcut = new KeyboardShortcut(((KeyboardShortcut) shortcuts.get(0)).getFirstKeyStroke(),
                        ((KeyboardShortcut) shortcuts.get(1)).getFirstKeyStroke());
            }
            myKeyBindings.put(firstKeystroke, action);
            return shortcut;
        }
        LispKeymap prefix = getPrefixKeymap(firstKeystroke);
        if (prefix != null) {
            return ((IdeaKeymap) prefix).defineKey(action, shortcuts, index + 1);
        }
        Core.error(Emacs4ijBundle.message("non.prefix.first.keystroke", shortcuts.toString(), firstKeystroke.toString()));
        return null;
    }

    private LispKeymap getPrefixKeymap (Shortcut shortcut) {
        if (!myKeyBindings.containsKey(shortcut))
            return null;
        return myKeyBindings.get(shortcut).getKeymap();
    }

    private String generateActionId (String binding) {
        return (Emacs4ijBundle.message("emacs4ij") + StringUtil.capitalizeWords(binding, "-", true, false)).replaceAll(" ", "");
    }

    private Map<Shortcut, String> myUnbound = new HashMap<>();
    
    private void unregisterShortcut (Shortcut shortcut) {
        String[] actionIds = myIdeaKeymap.getActionIds(shortcut);
        while (actionIds != ArrayUtil.EMPTY_STRING_ARRAY) {
            for (String actionId: actionIds) {
                myIdeaKeymap.removeShortcut(actionId, shortcut);
            }
            actionIds = myIdeaKeymap.getActionIds(shortcut);
        }
    }

    private Map<Shortcut, KeymapCell> getBindings() {
        return myKeyBindings;
    }

    private void registerAction (KeymapCell action, Shortcut shortcut) {
        if (!(shortcut instanceof KeyboardShortcut))
            return;
        if (action.getKeymap() == null) {
            registerKbdAction(action, (KeyboardShortcut) shortcut);
            return;
        }
        //only 2 keystrokes are supported
        if (((KeyboardShortcut) shortcut).getSecondKeyStroke() != null)
            return;
        KeyStroke first = ((KeyboardShortcut) shortcut).getFirstKeyStroke();
        IdeaKeymap inner = (IdeaKeymap) action.getKeymap();
        for (Map.Entry<Shortcut, KeymapCell> entry: inner.getBindings().entrySet()) {
            if (!(entry.getKey() instanceof KeyboardShortcut)
                    || (entry.getKey() instanceof KeyboardShortcut && ((KeyboardShortcut) entry.getKey()).getSecondKeyStroke() != null)
                    || entry.getValue().getKeymap() != null)
                continue;
            KeyStroke second = ((KeyboardShortcut) entry.getKey()).getFirstKeyStroke();
            registerKbdAction(entry.getValue(), new KeyboardShortcut(first, second));
        }
    }

    private void registerKbdAction(KeymapCell action, KeyboardShortcut shortcut) {
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
        for (Map.Entry<Shortcut, KeymapCell> entry: myKeyBindings.entrySet()) {
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
        for (KeymapCell cell: myKeyBindings.values()) {
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
    public KeymapCell getKeyBinding(StringOrVector key) {
        KeymapCell function = getKeyBinding(key.toKeyboardShortcutList());
        return (KeymapCell) Core.thisOrNil(function);
    }

    private KeymapCell getKeyBinding(List<Shortcut> shortcuts) {
        if (isEmpty() || shortcuts.isEmpty())
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

    private KeymapCell getKeyBinding (Shortcut shortcut) {
        KeymapCell function = myKeyBindings.get(shortcut);
        if (function != null)
            return function;
        if (myParent != null)
            return ((IdeaKeymap)myParent).getKeyBinding(shortcut);
        return LispSymbol.ourNil;
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
