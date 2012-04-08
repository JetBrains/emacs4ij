package org.jetbrains.emacs4ij.jelisp.parser.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 9:14 AM
 * To change this template use File | Settings | File Templates.
 */
public class EndOfLineException extends ParserException {
    public EndOfLineException () {
        super(JelispBundle.message("line.ended"));
    }
}
