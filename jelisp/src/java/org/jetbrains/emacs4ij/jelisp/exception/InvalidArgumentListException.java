package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/12/12
 * Time: 10:59 AM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidArgumentListException extends LispException {
    public InvalidArgumentListException (LispObject firstArg) {
        super(JelispBundle.message("invalid.arg.list", firstArg.toString()));
    }
}
