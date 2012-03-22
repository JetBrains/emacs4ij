package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/21/12
 * Time: 3:19 AM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap extends LispObject {
    void defineKey(KeymapCell action, LispStringOrVector key);
    void defineKey(String actionId, LispStringOrVector key);
    void defineKey(String actionId, Shortcut shortcut);
    void defineKey(KeymapCell action, Shortcut shortcut);
//    LispObject defineKey(Environment environment, LispObject function, LispStringOrVector key);
    KeymapCell getKeyBinding(LispStringOrVector key);
    LispSymbol getKeyBinding(Shortcut shortcut);
    void definePrefixCommand();

//    LispSymbol getKeyDefinition (Shortcut key);
//    LispObject getKeyDefinition (LispStringOrVector key, @Nullable @Optional LispObject acceptDefault);

    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
//    LispKeymap copy();
//    boolean isEmpty();
    
    String getName();
}
