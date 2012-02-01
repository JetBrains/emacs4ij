package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/28/11
 * Time: 4:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class InvalidControlLetterException extends LispException {
    public InvalidControlLetterException (char letter) {
        super("Invalid control letter: " + letter);
    }
}
