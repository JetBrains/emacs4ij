package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/22/12
 * Time: 5:57 PM
 * To change this template use File | Settings | File Templates.
 */
public class DoubleKeymapNameException extends LispException {
    public DoubleKeymapNameException(LispObject name) {
        super("Keymap with name " + name.toString() + " already exists!");
    }
}
