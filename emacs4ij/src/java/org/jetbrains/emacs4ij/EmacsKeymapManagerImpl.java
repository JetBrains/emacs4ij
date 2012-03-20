package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.util.ArrayUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.EmacsKeymapManager;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredKeymapException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/20/12
 * Time: 3:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class EmacsKeymapManagerImpl implements EmacsKeymapManager {
    private static EmacsAction ourAction = new EmacsAction();
    private static KeymapManager ourKeymapManager = KeymapManager.getInstance();
    private List<LispKeymap> myKeymaps = new ArrayList<>();
    private LispKeymap myCurrentKeyMap = null;         
    
    public EmacsKeymapManagerImpl() {}
    
    public LispSymbol getIdeaKeyBinding (Shortcut shortcut) {
        String[] actions = ourKeymapManager.getActiveKeymap().getActionIds(shortcut);
        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
            return null;
        if (actions.length != 1)
            throw new Attention();
        return new LispSymbol(actions[0]);
    }

    @Override
    public LispKeymap getActiveKeymap() {
        return myCurrentKeyMap;
    }

    @Override
    public LispKeymap createSparseKeymap (@Nullable LispObject prompt) {
        LispList keymap = (prompt != null && !prompt.equals(LispSymbol.ourNil))
                ? LispList.list(BuiltinsKey.ourKeyMapSymbol, prompt)
                : LispList.list(BuiltinsKey.ourKeyMapSymbol);
        registerKeymap(keymap);
        return keymap;
    }

    @Override
    public LispKeymap createKeymap (@Nullable LispObject prompt) {
        LispObject tail = LispSymbol.ourNil;
        if (prompt != null && !prompt.equals(LispSymbol.ourNil)) {
            tail = LispList.list(prompt);
        }
        LispList keymap = LispList.cons(BuiltinsKey.ourKeyMapSymbol,
                LispList.cons(new LispCharTable(BuiltinsKey.ourKeyMapSymbol, LispSymbol.ourNil), tail));
        registerKeymap(keymap);
        return keymap;
    }

    private void registerKeymap (LispKeymap keymap) {
        if (myCurrentKeyMap == null)
            myCurrentKeyMap = keymap;
        myKeymaps.add(keymap);
    }

    @Override
    public void setActiveKeymap (LispKeymap keymap) {
        if (!myKeymaps.contains(keymap))
            throw new UnregisteredKeymapException(keymap);
        myCurrentKeyMap = keymap;
    }

    private Shortcut toShortcut (LispStringOrVector key) {
        String s = key.toShortcutString();
        return KeyboardShortcut.fromString(s);
    }

//    @Override
//    public void defineKey(KeymapCell action, Shortcut shortcut) {
//        myKeyBindings.put(shortcut, action);
//        Shortcut[] shortcuts = ourAction.getShortcutSet().getShortcuts();
//        if (shortcuts.length == 0)
//            ourAction.registerCustomShortcutSet(new CustomShortcutSet(shortcut),
//                    getFrameComponent());
//        else {
//            ArrayList<Shortcut> a = new ArrayList<>();
//            a.addAll(Arrays.asList(shortcuts));
//            a.add(shortcut);
//            ourAction.registerCustomShortcutSet(new CustomShortcutSet(a.toArray(new Shortcut[a.size()])),
//                    getFrameComponent());
//        }
//    }
//
//
//
//
//    @Override
//    public LispSymbol getKeyBinding(Shortcut shortcut) {
//        KeymapCell function = myKeyBindings.get(shortcut);
//
//        if (function != null) {
//            if (function instanceof LispSymbol)
//                return (LispSymbol) function;
//            //todo: get part of shortcut which corresponds to prefix, cut it and go on
//            return null;
//
//        }
//        if (myParent != null)
//            return myParent.getKeyBinding(shortcut);
//
//        String[] actions = ourKeymapManager.getActiveKeymap().getActionIds(shortcut);
//        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
//            return null;
//        if (actions.length != 1)
//            throw new Attention();
//        return new LispSymbol(actions[0]);
//    }

//    private JComponent getFrameComponent() {
//        if (GlobalEnvironment.INSTANCE.getSelectedFrame() == null)
//            return null;
//        return GlobalEnvironment.INSTANCE.getSelectedFrame().getComponent();
//    }


}