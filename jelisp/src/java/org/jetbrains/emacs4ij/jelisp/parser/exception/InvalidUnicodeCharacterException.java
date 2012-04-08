package org.jetbrains.emacs4ij.jelisp.parser.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/7/12
 * Time: 8:34 PM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidUnicodeCharacterException extends ParserException {
    public InvalidUnicodeCharacterException () {
        super(JelispBundle.message("invalid.unicode.char"));
    }
}
