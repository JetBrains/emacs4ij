package org.jetbrains.emacs4ij;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymapFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/22/12
 * Time: 4:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class KeymapCreator implements LispKeymapFactory {
    @Override
    public LispKeymap createKeymap(@Nullable LispObject name, @Nullable LispKeymap parent) {
        return new IdeaKeymap(name, parent);
    }
}
