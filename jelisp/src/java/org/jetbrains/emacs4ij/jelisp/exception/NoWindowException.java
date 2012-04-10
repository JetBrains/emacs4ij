package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 4/9/12
 * Time: 12:04 AM
 * To change this template use File | Settings | File Templates.
 */
public class NoWindowException extends LispException {
    public NoWindowException (LispFrame frame, LispWindow window) {
        super(JelispBundle.message("no.such.window.in.frame", window.toString(), frame.toString()));
    }
}
