package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/21/12
 * Time: 3:19 AM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap extends LispObject {
    LispObject defineKey(Environment environment, LispObject function, LispStringOrVector key);
    LispSymbol getKeyDefinition (Shortcut key);
    LispObject getKeyDefinition (LispStringOrVector key, @Nullable @Optional LispObject acceptDefault);
    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
    LispKeymap copy();
    boolean isEmpty();
}
