package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import com.intellij.openapi.keymap.Keymap;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/13/12
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap extends KeymapCell {
    void defineKey (String actionId, Shortcut sc);
    void defineKey(KeymapCell action, Shortcut shortcut);
    void defineKey(String actionId, LispStringOrVector key);
    void defineKey(KeymapCell action, LispStringOrVector key);

    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
    void definePrefixCommand();
    Keymap getKeymap();
    LispSymbol getKeyBinding (Shortcut shortcut);
    KeymapCell getKeyBinding(LispStringOrVector key);
    

}
