package org.jetbrains.emacs4ij.jelisp.elisp;

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
    void bindActions(); //for my KeymapManager
    KeymapCell getKeyBinding(LispStringOrVector key);
    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
    LispKeymap copy();
    boolean isEmpty();
    String getName();
}
