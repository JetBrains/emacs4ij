package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 2:30 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoOpenedWindowException extends LispException {
    public NoOpenedWindowException() {
        super(JelispBundle.message("no.opened", "window"));
    }
}
