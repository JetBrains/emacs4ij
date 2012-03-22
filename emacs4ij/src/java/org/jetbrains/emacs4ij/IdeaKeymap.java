package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.CustomShortcutSet;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.util.ArrayUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispStringOrVector;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.isNil;

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
    private static EmacsAction ourAction = new EmacsAction();
    
//    private LispList myEmacsKeymap;
//    private LispSymbol myKeymapSymbol = new LispSymbol("keymap");
    

    public IdeaKeymap(@Nullable LispObject name, LispKeymap parent) {
        myParent = parent;
        myName = isNil(name) ? Emacs4ijBundle.message("empty.keymap.name") : name.toString();
//        myEmacsKeymap = isNil(name) ? LispList.list(myKeymapSymbol) : LispList.list(myKeymapSymbol, name);
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

    @Override
    public void defineKey(String actionId, LispStringOrVector key) {
        defineKey(new LispSymbol(actionId), key);
    }

    @Override
    public void defineKey(String actionId, Shortcut shortcut) {
        defineKey(new LispSymbol(actionId), shortcut);
    }

    @Override
    public void defineKey(KeymapCell action, Shortcut shortcut) {
        myKeyBindings.put(shortcut, action);
        Shortcut[] shortcuts = ourAction.getShortcutSet().getShortcuts();
        if (shortcuts.length == 0)
            ourAction.registerCustomShortcutSet(new CustomShortcutSet(shortcut),
                    getFrameComponent());
        else {
            ArrayList<Shortcut> a = new ArrayList<>();
            a.addAll(Arrays.asList(shortcuts));
            a.add(shortcut);
            ourAction.registerCustomShortcutSet(new CustomShortcutSet(a.toArray(new Shortcut[a.size()])),
                    getFrameComponent());
        }
    }


    @Override
    public LispKeymap getParent() {
        return myParent;
    }

    @Override
    public void setParent(@Nullable LispKeymap parent) {
        myParent = parent;
        for (KeymapCell cell: myKeyBindings.values()) {
            if (cell instanceof LispKeymap)
                ((LispKeymap) cell).setParent(parent);
        }
    }

    @Override
    public void definePrefixCommand() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public KeymapCell getKeyBinding(LispStringOrVector key) {
        KeymapCell function = getKeyBinding(toShortcut(key));
        return function == null ? LispSymbol.ourNil : function;
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
