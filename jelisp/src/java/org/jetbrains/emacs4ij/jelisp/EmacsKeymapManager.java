package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.keymap.KeymapUtil;
import com.intellij.openapi.keymap.impl.DefaultKeymap;
import com.intellij.openapi.keymap.impl.KeymapManagerImpl;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymapFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleKeymapNameException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredKeymapException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;

import java.util.ArrayList;
import java.util.Arrays;
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
    private LispKeymapFactory myKeymapFactory;
    private int myNoNameKeymapCounter = 0;
    private final Keymap myIdeaEmacsKeymap;

    public EmacsKeymapManager(LispKeymapFactory keymapFactory) {
        myKeymapFactory = keymapFactory;
        Keymap tmp;
        try {     
            tmp = (Keymap) CollectionUtils.find(
                Arrays.asList(DefaultKeymap.getInstance().getKeymaps()),
                new Predicate() {
                    @Override
                    public boolean evaluate(Object o) {
                        return KeymapUtil.isEmacsKeymap((Keymap) o);
                    }
                });
        } catch (NullPointerException e) {
            tmp = null;
        }
        myIdeaEmacsKeymap = tmp;
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
        if (myKeymapFactory == null) //todo: this is only for test!
            return null;
        if (BuiltinPredicates.isNil(name))
            name = new LispInteger(myNoNameKeymapCounter++);
        else if (!isUniqueKeymapName(name.toString()))
            throw new DoubleKeymapNameException(name);
        LispKeymap keymap = myKeymapFactory.createKeymap(name, parent);
        myKeymaps.add(keymap);
        if (myCurrentKeyMap == null)
            setActiveKeymap(keymap);
        return keymap;
    }

    public void setActiveKeymap (LispKeymap keymap) {
        if (!myKeymaps.contains(keymap))
            throw new UnregisteredKeymapException(keymap);
        myCurrentKeyMap = keymap;
        assignMyBindingsToIdeaActiveKeymap();
    }

    private void assignMyBindingsToIdeaActiveKeymap() {
        ((KeymapManagerImpl) KeymapManager.getInstance()).setActiveKeymap(myIdeaEmacsKeymap);
        myCurrentKeyMap.bindActions();
    }

}
