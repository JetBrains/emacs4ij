package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymapFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleKeymapNameException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredKeymapException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/16/12
 * Time: 11:58 AM
 * To change this template use File | Settings | File Templates.
 */
public class EmacsKeymapManager {
    private List<LispKeymap> myKeymaps = new ArrayList<>();
    private LispKeymap myCurrentKeyMap = null;
    private final LispKeymapFactory myKeymapFactory;

    public EmacsKeymapManager (LispKeymapFactory keymapFactory) {
        myKeymapFactory = keymapFactory;
    }

    LispKeymap getActiveKeymap() {
        return myCurrentKeyMap;
    }

    public LispKeymap createKeymap (@Nullable LispObject name) {
        return createKeymap(name, null);
    }

    private boolean isUniqueKeymapName (String name) {
        for (LispKeymap keymap: myKeymaps) {
            if (keymap.getName().equals(name))
                return false;
        }
        return true;
    }

    private LispKeymap createKeymap (@Nullable LispObject name, @Nullable LispKeymap parent) {
        if (myKeymapFactory == null) //todo: this is only for test!
            return null;
        else if (!BuiltinPredicates.isNil(name) && !isUniqueKeymapName(name.toString()))
            throw new DoubleKeymapNameException(name);
        LispKeymap keymap = myKeymapFactory.createKeymap(name, parent);
        myKeymaps.add(keymap);
        if (myCurrentKeyMap == null)
            setActiveKeymap(keymap);
        return keymap;
    }

    public void setActiveKeymap (@NotNull LispKeymap keymap) {
        if (!myKeymaps.contains(keymap))
            throw new UnregisteredKeymapException(keymap);
        assignBindingsToIdeaActiveKeymap(keymap);
    }

    public void setActiveKeymap(String name) {
        LispKeymap keymap = getByName(name);
        if (keymap == null)
            throw new UnregisteredKeymapException(name);
        assignBindingsToIdeaActiveKeymap(keymap);
    }

    private void assignBindingsToIdeaActiveKeymap(@NotNull LispKeymap newCurrent) {
        if (myCurrentKeyMap == newCurrent)
            return;
        newCurrent.bindActions(myCurrentKeyMap);
        myCurrentKeyMap = newCurrent;
    }

    public boolean isKeymapActive (LispKeymap keymap) {
        return myCurrentKeyMap != null && myCurrentKeyMap == keymap;
    }

    private LispKeymap getByName (String name) {
        for (LispKeymap keymap: myKeymaps) {
            if (keymap.getName().equals(name))
                return keymap;
        }
        return null;
    }
}
