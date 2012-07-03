package org.jetbrains.emacs4ij.jelisp.platform_dependent;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.StringOrVector;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/21/12
 * Time: 3:19 AM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymap extends KeymapCell {
    void defineKey(LispObject action, StringOrVector key);
    void bindActions (@Nullable LispKeymap current); //for my KeymapManager
    LispObject getKeyBinding(LispObject key);
    LispKeymap getParent();
    void setParent(@Nullable LispKeymap parent);
    LispKeymap copy();
    boolean isEmpty();
    String getName();
}
