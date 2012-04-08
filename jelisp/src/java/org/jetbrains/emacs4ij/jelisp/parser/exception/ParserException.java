package org.jetbrains.emacs4ij.jelisp.parser.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/6/12
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class ParserException extends LispException {
    protected ParserException (String message) {
        super(message);
    }

    public ParserException (String filename, int lineNumber, String message, String code) {
        super(JelispBundle.message("parser.error", filename, lineNumber, message, code));
    }
}
