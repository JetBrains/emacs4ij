package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/31/11
 * Time: 10:30 AM
 * To change this template use File | Settings | File Templates.
 */
public class NoOpenedBufferException extends LispException {
    public NoOpenedBufferException() {
        super(JelispBundle.message("no.opened.buffer"));
    }
}
