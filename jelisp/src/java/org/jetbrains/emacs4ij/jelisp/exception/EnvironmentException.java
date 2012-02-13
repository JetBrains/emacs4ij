package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/5/11
 * Time: 3:45 PM
 * To change this template use File | Settings | File Templates.
 */
public class EnvironmentException extends LispException {
    public EnvironmentException (String message) {
        super(message);
    }
}
