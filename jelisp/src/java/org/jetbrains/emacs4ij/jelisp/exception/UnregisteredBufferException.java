package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 3:29 PM
 * To change this template use File | Settings | File Templates.
 */
public class UnregisteredBufferException extends LispException {
    public UnregisteredBufferException (String bufferName) {
        super(JelispBundle.message("unregistered.item", "buffer", bufferName));
    }
}
