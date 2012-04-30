package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/28/12
 * Time: 4:53 PM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidBackslashInReplacementException extends LispException {
    public InvalidBackslashInReplacementException () {
        super(JelispBundle.message("invalid.backslash.in.replacement"));
    }

}
