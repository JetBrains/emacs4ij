package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 1:21 PM
 * To change this template use File | Settings | File Templates.
 */
public class InternalError extends RuntimeException {
    public InternalError (String message) {
        super(message);
    }
}