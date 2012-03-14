package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.keymap.Keymap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/13/12
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap {
    void defineKey(String actionId, LispStringOrVector key);
    void defineKey(LispSymbol actionId, LispStringOrVector key);
    LispKeymap getParent();
    void setParent();
    void definePrefixCommand();
    LispSymbol getKeyBinding(LispStringOrVector key);
    Keymap getKeymap();
}
