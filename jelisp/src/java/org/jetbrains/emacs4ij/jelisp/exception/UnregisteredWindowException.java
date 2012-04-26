package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 3:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class UnregisteredWindowException extends LispException {
    public UnregisteredWindowException (String window) {
        super(JelispBundle.message("unregistered.item", "window", window));
    }
}
