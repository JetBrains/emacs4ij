package org.jetbrains.emacs4ij.ide;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispKeymapFactory;

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
