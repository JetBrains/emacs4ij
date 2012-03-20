package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/13/12
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap extends KeymapCell {
    void defineKey(Environment environment, LispObject function, LispStringOrVector key);
    LispObject accessKeymap(LispObject idx, boolean tOk, boolean noInherit);
    LispSymbol getKeyDefinition (Shortcut key);
    LispObject getKeyDefinition (LispStringOrVector key, @Nullable @Optional LispObject acceptDefault);
    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
    LispKeymap copy();
//    Check whether THIS is one of MAPS parents.
    boolean keymapMemberP(LispObject maps);
}
