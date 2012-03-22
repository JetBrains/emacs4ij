package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymapFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredKeymapException;

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
    
    public KeymapManager (LispKeymapFactory keymapFactory) {
        myKeymapFactory = keymapFactory;        
    }
    
    LispKeymap getActiveKeymap() {
        return myCurrentKeyMap;        
    }
    
    public LispKeymap createKeymap (@Nullable LispObject name) {
        return createKeymap(name, null);
    }

    public LispKeymap createKeymap (@Nullable LispObject name, @Nullable LispKeymap parent) {
        //todo: this hack is only for test!
        if (myKeymapFactory == null)
            return null;

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
    }
    
}
