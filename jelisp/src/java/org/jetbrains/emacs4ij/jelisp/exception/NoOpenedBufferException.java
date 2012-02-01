package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/31/11
 * Time: 10:30 AM
 * To change this template use File | Settings | File Templates.
 */
public class NoOpenedBufferException extends LispException {
    public NoOpenedBufferException() {
        super("No buffer is currently opened!");
    }
}
