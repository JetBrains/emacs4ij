package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 2:25 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoFrameMinibuffer extends LispException {
    public NoFrameMinibuffer (String frame) {
        super(JelispBundle.message("no.frame.minibuffer", frame));
    }
}
