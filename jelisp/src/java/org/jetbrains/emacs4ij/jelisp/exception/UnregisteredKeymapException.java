package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispKeymap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/16/12
 * Time: 1:13 PM
 * To change this template use File | Settings | File Templates.
 */
public class UnregisteredKeymapException extends LispException {
    public UnregisteredKeymapException (LispKeymap keymap) {
        super(JelispBundle.message("unregistered.keymap", keymap.toString()));
    }

    public UnregisteredKeymapException (String keymap) {
        super(JelispBundle.message("unregistered.keymap", keymap));
    }
}
