package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/12/12
 * Time: 1:02 PM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidHashTableParameterException extends LispException {
    public InvalidHashTableParameterException(String parameter, LispObject value) {
        super(JelispBundle.message("invalid.hash.table.param", parameter, value.toString()));
    }

    public InvalidHashTableParameterException(String parameter, String value) {
        super(JelispBundle.message("invalid.hash.table.param", parameter, value));
    }

    public InvalidHashTableParameterException(String parameter, int value) {
        super(JelispBundle.message("invalid.hash.table.param", parameter, value));
    }

    public InvalidHashTableParameterException(String parameter, float value) {
        super(JelispBundle.message("invalid.hash.table.param", parameter, value));
    }
}
