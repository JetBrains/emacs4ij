package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 4/1/12
 * Time: 3:16 PM
 * To change this template use File | Settings | File Templates.
 */
public class ReadException extends LispException {
    public ReadException (String message) {
        super(JelispBundle.message("read.error", message));
    }
}
