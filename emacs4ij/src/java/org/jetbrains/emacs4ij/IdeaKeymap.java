package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.keymap.Keymap;
import com.intellij.openapi.keymap.KeymapManager;
import com.intellij.openapi.keymap.impl.KeymapImpl;
import com.intellij.util.ArrayUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSequence;
import org.jetbrains.emacs4ij.jelisp.elisp.LispStringOrVector;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 3:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaKeymap implements LispKeymap {
    private Keymap myKeymap;
    
    public IdeaKeymap() {
        myKeymap = ((KeymapImpl) KeymapManager.getInstance().getActiveKeymap()).deriveKeymap();
    }
    
    public IdeaKeymap (Keymap keymap) {
        myKeymap = keymap;
    }

    @Override
    public void defineKey(String actionId, LispStringOrVector key) {
        myKeymap.addShortcut(actionId, toShortcut(key));
    }

    @Override
    public void defineKey(LispSymbol actionId, LispStringOrVector key) {
        defineKey(actionId.getName(), key);
    }

    @Override
    public LispKeymap getParent() {
        return new IdeaKeymap(myKeymap.getParent());
    }

    @Override
    public void setParent() {
        throw new NotImplementedException();
    }

    @Override
    public void definePrefixCommand() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispSymbol getKeyBinding(LispStringOrVector key) {
        String[] actions = myKeymap.getActionIds(toShortcut(key));
        if (actions == ArrayUtil.EMPTY_STRING_ARRAY)
            return LispSymbol.ourNil;
        if (actions.length != 1)
            throw new Attention();
        return new LispSymbol(actions[0]);
    }

    @Override
    public Keymap getKeymap() {
        return myKeymap;
    }

    private Shortcut toShortcut (LispStringOrVector key) {
        String s = ((LispSequence)key).toCharString();
        return KeyboardShortcut.fromString(s);
    }
    
    @Override
    public String toString() {
        throw new NotImplementedException();
    }
}
