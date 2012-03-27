package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ArrayUtil;
import com.rits.cloning.Cloner;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispStringOrVector;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

import javax.swing.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 3:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaKeymap implements LispKeymap, KeymapCell {
    private String myName;
    private LispKeymap myParent = null;
    private Map<Shortcut, KeymapCell> myKeyBindings = new HashMap<>();
    protected static KeymapManager ourKeymapManager = KeymapManager.getInstance();

    public IdeaKeymap(@Nullable LispObject name, @Nullable LispKeymap parent) {
        myParent = parent;
        myName = name == null ? Emacs4ijBundle.message("empty.keymap.name") : name.toString();
//        myName = name instanceof LispInteger
//                ? Emacs4ijBundle.message("empty.keymap.name") + name.toString()
//                : name.toString();
    }

    private JComponent getFrameComponent() {
        if (GlobalEnvironment.INSTANCE.getSelectedFrame() == null)
            return null;
        return GlobalEnvironment.INSTANCE.getSelectedFrame().getComponent();
    }

    @Override
    public void defineKey(KeymapCell action, LispStringOrVector key) {
        defineKey(action, key.toKeyboardShortcutList());
    }

    private void defineKey(KeymapCell action, List<Shortcut> shortcuts) {
        if (shortcuts.isEmpty())
            return;
        Shortcut firstKeystroke = shortcuts.get(0);
        if (shortcuts.size() == 1) {
            if (action instanceof LispSymbol && ((LispSymbol) action).isFunction() && ((LispSymbol) action).getFunction() instanceof LispKeymap)
                action = (KeymapCell) ((LispSymbol) action).getFunction();
            myKeyBindings.put(firstKeystroke, action);
            registerAction(action, firstKeystroke);
            return;
        }
        if (isPrefixKey(firstKeystroke)) {
            ((IdeaKeymap)myKeyBindings.get(firstKeystroke)).defineKey(action, shortcuts.subList(1, shortcuts.size()));
            return;
        }
        BuiltinsCore.error(Emacs4ijBundle.message("non.prefix.first.keystroke", shortcuts.toString(), firstKeystroke.toString()));
    }

    private boolean isPrefixKey (Shortcut shortcut) {
        return myKeyBindings.containsKey(shortcut) && myKeyBindings.get(shortcut) instanceof LispKeymap;
    }

    private String generateActionId (String binding) {
        return Emacs4ijBundle.message("emacs4ij") + ' ' + StringUtil.capitalizeWords(binding, "-", true, false);
    }

    private void unregisterShortcut (Shortcut shortcut) {
        Keymap activeKeymap = ourKeymapManager.getActiveKeymap();
        String[] actionIds = activeKeymap.getActionIds(shortcut);
        while (actionIds != ArrayUtil.EMPTY_STRING_ARRAY) {
            for (String actionId: actionIds)
                activeKeymap.removeShortcut(actionId, shortcut);
            actionIds = activeKeymap.getActionIds(shortcut);
        }
    }

    private void registerAction (KeymapCell action, Shortcut shortcut) {
        if (action instanceof LispSymbol) {
            ActionManager actionManager = ActionManager.getInstance();
            String id = generateActionId(action.toString());
            if (actionManager.getActionIds(id).length != 0)
                actionManager.unregisterAction(id);
            actionManager.registerAction(id, new EmacsAction((LispSymbol) action), PluginId.getId(Emacs4ijBundle.message("emacs4ij")));
            unregisterShortcut(shortcut);
            ourKeymapManager.getActiveKeymap().addShortcut(id, shortcut);
        }
    }

    @Override
    public void bindActions () {
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
            BuiltinsCore.error("Cyclic keymap inheritance");
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
    public KeymapCell getKeyBinding(LispStringOrVector key) {
        KeymapCell function = getKeyBinding(key.toKeyboardShortcutList());
        return (KeymapCell) BuiltinsCore.thisOrNil(function);
    }

    private KeymapCell getKeyBinding(List<Shortcut> shortcuts) {
        if (shortcuts.isEmpty())
            return null;
        Shortcut firstKeystroke = shortcuts.get(0);
        if (shortcuts.size() == 1)
            return getKeyBinding(firstKeystroke);
        if (isPrefixKey(firstKeystroke)) {
            return ((IdeaKeymap)myKeyBindings.get(firstKeystroke)).getKeyBinding(shortcuts.subList(1, shortcuts.size()));
        }
        return null;
    }

    private KeymapCell getKeyBinding (Shortcut shortcut) {
        KeymapCell function = myKeyBindings.get(shortcut);
        if (function != null)
            return function;
        if (myParent != null)
            return ((IdeaKeymap)myParent).getKeyBinding(shortcut);
        //get idea binding
        String[] actions = ourKeymapManager.getActiveKeymap().getActionIds(shortcut);
        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
            return null;
        if (actions.length != 1)
            throw new Attention();
        return new LispSymbol(actions[0]);
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
}
