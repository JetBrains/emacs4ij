package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/8/12
 * Time: 11:18 AM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidReadSyntax extends LispException {
    public InvalidReadSyntax (String message) {
        super("'(invalid-read-syntax \"" + message + "\")");
    }
}
