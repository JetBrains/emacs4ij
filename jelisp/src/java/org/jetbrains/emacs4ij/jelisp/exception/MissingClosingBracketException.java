package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 7:50 PM
 * To change this template use File | Settings | File Templates.
 */

public class MissingClosingBracketException extends LispException {
    public MissingClosingBracketException() {
         super(JelispBundle.message("missing.closing.bracket"));
    }
}
