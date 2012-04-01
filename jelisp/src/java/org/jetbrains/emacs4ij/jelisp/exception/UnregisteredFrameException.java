package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 4/1/12
 * Time: 3:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class UnregisteredFrameException extends LispException {
    public UnregisteredFrameException (LispFrame frame) {
        super(JelispBundle.message("unregistered.frame", frame.toString()));
    }
}
