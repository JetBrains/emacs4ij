package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.CustomShortcutSet;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.keymap.impl.KeymapImpl;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispStringOrVector;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

import java.util.ArrayList;
import java.util.Arrays;
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
    private Keymap myKeymap;
    private String myName;
    private LispKeymap myParent = null;
    private Map<Shortcut, String> myKeyBindings = new HashMap<>();
    protected static KeymapManager ourKeymapManager = KeymapManager.getInstance();
    private static EmacsAction ourAction = new EmacsAction();
    
    //global keymap, derived from Idea active keymap
    public IdeaKeymap(String name) {
       this(name, new IdeaKeymap(ourKeymapManager.getActiveKeymap()));
    }

    public IdeaKeymap(String name, LispKeymap parent) {
        myKeymap = new KeymapImpl();
        myParent = parent;
        myName = name;
//        ((KeymapImpl)myKeymap).setName(name);
//        ((KeymapManagerImpl)ourKeymapManager).addKeymap(myKeymap);
    }
    
    public IdeaKeymap (Keymap keymap) {
        myKeymap = keymap;
    }

    @Override
    public void defineKey(String functionName, LispStringOrVector key) {
        Shortcut shortcut = toShortcut(key);
        myKeyBindings.put(shortcut, functionName);
        Shortcut[] shortcuts = ourAction.getShortcutSet().getShortcuts();
        if (shortcuts.length == 0)
            ourAction.registerCustomShortcutSet(new CustomShortcutSet(shortcut), 
                    GlobalEnvironment.INSTANCE.getSelectedFrame().getComponent());
        else {
            ArrayList<Shortcut> a = new ArrayList<>();
            a.addAll(Arrays.asList(shortcuts));
            a.add(shortcut);
            ourAction.registerCustomShortcutSet(new CustomShortcutSet(a.toArray(new Shortcut[a.size()])),
                    GlobalEnvironment.INSTANCE.getSelectedFrame().getComponent());
        }
//        myKeymap.addShortcut("EmacsAction", shortcut);
    }

    @Override
    public void defineKey(LispSymbol actionId, LispStringOrVector key) {
        defineKey(actionId.getName(), key);
    }

    @Override
    public LispKeymap getParent() {
        return myParent;
//        return new IdeaKeymap(myKeymap.getParent());
    }

    @Override
    public void setParent(@Nullable LispKeymap parent) {
        myParent = parent;
//        throw new NotImplementedException("IdeaKeymap.setParent()");
    }

    @Override
    public void definePrefixCommand() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispSymbol getKeyBinding(LispStringOrVector key) {
        String function = getKeyBinding(toShortcut(key));
        return function == null ? LispSymbol.ourNil : new LispSymbol(function);
    }

    @Override
    public Keymap getKeymap() {
        return myKeymap;
    }

    @Override
    public String getKeyBinding(Shortcut shortcut) {
        String function = myKeyBindings.get(shortcut);
        if (function == null) {
            //todo: look up in parent
            return null;            
        }
        return function;

//        String[] actions = myKeymap.getActionIds(shortcut);
//        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
//            return null;
//        if (actions.length != 1)
//            throw new Attention();
//        return actions[0];
    }

    private Shortcut toShortcut (LispStringOrVector key) {
        String s = key.toShortcutString();
        return KeyboardShortcut.fromString(s);
    }
    
    @Override
    public String toString() {
        return myName;
//        return myKeymap.getPresentableName();
//        throw new NotImplementedException("IdeaKeyMap.toString()");
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }
}
