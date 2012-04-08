package org.jetbrains.emacs4ij.jelisp.parser.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/30/12
 * Time: 12:30 PM
 * To change this template use File | Settings | File Templates.
 */
public class EndOfFileException extends ParserException {
    public EndOfFileException () {
        super(JelispBundle.message("file.ended.while.parsing"));
    }
}
