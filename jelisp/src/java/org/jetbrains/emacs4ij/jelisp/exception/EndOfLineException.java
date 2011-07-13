package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 9:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class EndOfLineException extends LispException {
    public EndOfLineException () {
        super("Index reached the end of line");
    }
}
