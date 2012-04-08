package org.jetbrains.emacs4ij.jelisp.parser.exception;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/6/12
 * Time: 12:36 PM
 * To change this template use File | Settings | File Templates.
 */
@org.jetbrains.emacs4ij.jelisp.exception.Error("invalid-read-syntax")
public class InvalidReadSyntaxDot extends ParserException {
    public InvalidReadSyntaxDot () {
        super("'(invalid-read-syntax \".\")");
    }
}
