package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/24/12
 * Time: 5:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoOpenedBufferException extends LispException {
    public NoOpenedBufferException() {
        super(JelispBundle.message("no.opened", "buffer"));
    }
}
