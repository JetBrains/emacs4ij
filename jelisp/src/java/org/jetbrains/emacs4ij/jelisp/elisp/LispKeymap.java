package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.keymap.Keymap;
import org.jetbrains.annotations.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/13/12
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap extends LispObject {
    void defineKey(String functionName, LispStringOrVector key);
    void defineKey(LispSymbol actionId, LispStringOrVector key);
    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
    void definePrefixCommand();
    LispSymbol getKeyBinding(LispStringOrVector key);
    Keymap getKeymap();
    String getKeyBinding (Shortcut shortcut);


}
