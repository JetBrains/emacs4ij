package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/22/11
 * Time: 4:42 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoBufferException extends LispException {
    public NoBufferException (String bufferName) {
        super(JelispBundle.message("no.buffer", bufferName));
    }
}
