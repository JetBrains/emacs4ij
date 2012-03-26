package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.ArrayUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

import javax.swing.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 3:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaKeymap implements LispKeymap {
    private String myName;
    private LispKeymap myParent = null;
    private Map<Shortcut, KeymapCell> myKeyBindings = new HashMap<>();
    protected static KeymapManager ourKeymapManager = KeymapManager.getInstance();
    
    public IdeaKeymap(@NotNull LispObject name, @Nullable LispKeymap parent) {
        myParent = parent;
        myName = name instanceof LispInteger
                ? Emacs4ijBundle.message("empty.keymap.name") + name.toString()
                : name.toString();
    }

    private JComponent getFrameComponent() {
        if (GlobalEnvironment.INSTANCE.getSelectedFrame() == null)
            return null;
        return GlobalEnvironment.INSTANCE.getSelectedFrame().getComponent();
    }

    @Override
    public void defineKey(KeymapCell action, LispStringOrVector key) {
        defineKey(action, toShortcut(key));
    }

    private String generateActionId (String binding) {
        return Emacs4ijBundle.message("emacs4ij") + ' ' + StringUtil.capitalizeWords(binding, "-", true, false);   //"emacs4ij.action."+ myName + "." +
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

    @Override
    public void defineKey(KeymapCell action, Shortcut shortcut) {
        myKeyBindings.put(shortcut, action);
        registerAction(action, shortcut);
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
    public String getName() {
        return myName;
    }

    @Override
    public void definePrefixCommand() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public KeymapCell getKeyBinding(LispStringOrVector key) {
        KeymapCell function = getKeyBinding(toShortcut(key));
        return (KeymapCell) BuiltinsCore.thisOrNil(function);
    }

    @Override
    public LispSymbol getKeyBinding(Shortcut shortcut) {
        KeymapCell function = myKeyBindings.get(shortcut);

        if (function != null) {
            if (function instanceof LispSymbol)
                return (LispSymbol) function;
            //todo: get part of shortcut which corresponds to prefix, cut it and go on
            return null;

        }
        if (myParent != null)
            return myParent.getKeyBinding(shortcut);
        String[] actions = ourKeymapManager.getActiveKeymap().getActionIds(shortcut);
        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
            return null;
        if (actions.length != 1)
            throw new Attention();
        return new LispSymbol(actions[0]);
    }

    private Shortcut toShortcut (LispStringOrVector key) {
        String s = key.toShortcutString();
        return KeyboardShortcut.fromString(s);
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
