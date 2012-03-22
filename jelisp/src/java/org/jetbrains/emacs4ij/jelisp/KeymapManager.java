package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymapFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleKeymapNameException;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;
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
public class KeymapManager {
    private List<LispKeymap> myKeymaps = new ArrayList<>();
    private LispKeymap myCurrentKeyMap = null;
    private LispKeymapFactory myKeymapFactory;
    private int myNoNameKeymapCounter = 0;

    public KeymapManager (LispKeymapFactory keymapFactory) {
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

    public LispKeymap createKeymap (@Nullable LispObject name, @Nullable LispKeymap parent) {
        //todo: this hack is only for test!
        if (myKeymapFactory == null)
            return null;

        if (BuiltinPredicates.isNil(name))
            name = new LispInteger(myNoNameKeymapCounter++);
        else if (!isUniqueKeymapName(name.toString()))
            throw new DoubleKeymapNameException(name);
        LispKeymap keymap = myKeymapFactory.createKeymap(name, parent);

        if (myCurrentKeyMap == null)
            myCurrentKeyMap = keymap;
        myKeymaps.add(keymap);
        return keymap;
    }

    public void setActiveKeymap (LispKeymap keymap) {
        if (!myKeymaps.contains(keymap))
            throw new UnregisteredKeymapException(keymap);
        myCurrentKeyMap = keymap;
        assignMyBindingsToIdeaActiveKeymap();
    }

    private void assignMyBindingsToIdeaActiveKeymap() {
        throw new NotImplementedException("assignMyBindingsToIdeaActiveKeymap");
    }

}
