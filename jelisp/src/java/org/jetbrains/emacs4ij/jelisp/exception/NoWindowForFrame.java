package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/26/12
 * Time: 12:35 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoWindowForFrame extends LispException {
    public NoWindowForFrame(String frame) {
        super(JelispBundle.message("no.window.on.frame", frame));
    }
}
