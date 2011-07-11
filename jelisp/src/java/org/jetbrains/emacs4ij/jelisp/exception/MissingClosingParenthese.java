package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 7:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class MissingClosingParenthese extends LispException {
    public MissingClosingParenthese () {
        super("Missing closing parenthese");
    }
}
