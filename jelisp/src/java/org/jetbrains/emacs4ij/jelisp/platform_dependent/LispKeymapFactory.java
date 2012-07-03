package org.jetbrains.emacs4ij.jelisp.platform_dependent;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/15/12
 * Time: 4:09 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispKeymapFactory {
    LispKeymap createKeymap (@Nullable LispObject name, @Nullable LispKeymap parent);
}
