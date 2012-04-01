package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 4/1/12
 * Time: 12:29 PM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidFormatOperationException extends LispException {
    public InvalidFormatOperationException (char format) {
        super("Invalid format operation %" + format);
    }
}
