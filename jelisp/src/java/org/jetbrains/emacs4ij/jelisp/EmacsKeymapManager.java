package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/16/12
 * Time: 11:58 AM
 * To change this template use File | Settings | File Templates.
 */
public interface EmacsKeymapManager {
    LispKeymap getActiveKeymap();
    LispKeymap createSparseKeymap (@Nullable LispObject prompt);
    LispKeymap createKeymap (@Nullable LispObject prompt);
    void setActiveKeymap (LispKeymap keymap);
}
