package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 4:00 PM
 * To change this template use File | Settings | File Templates.
 */
public class UnregisteredFrameException extends LispException {
    public UnregisteredFrameException (String frame) {
        super(JelispBundle.message("unregistered.item", "frame", frame));
    }
}