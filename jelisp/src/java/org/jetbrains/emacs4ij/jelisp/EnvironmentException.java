package org.jetbrains.emacs4ij.jelisp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/5/11
 * Time: 3:45 PM
 * To change this template use File | Settings | File Templates.
 */
public class EnvironmentException extends RuntimeException {
    public EnvironmentException (String message) {
        super(message);
    }
}
