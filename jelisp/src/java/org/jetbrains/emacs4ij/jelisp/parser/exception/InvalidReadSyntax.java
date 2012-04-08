package org.jetbrains.emacs4ij.jelisp.parser.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/8/12
 * Time: 11:18 AM
 * To change this template use File | Settings | File Templates.
 */
@org.jetbrains.emacs4ij.jelisp.exception.Error("invalid-read-syntax")
public class InvalidReadSyntax extends ParserException {
    public InvalidReadSyntax (String message) {
        super("'(invalid-read-syntax \"" + message + "\")");
    }
}
