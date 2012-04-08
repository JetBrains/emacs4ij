package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/28/11
 * Time: 4:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidControlLetterException extends LispException {
    public InvalidControlLetterException (char letter) {
        super(JelispBundle.message("control.letter.error", letter));
    }
}
