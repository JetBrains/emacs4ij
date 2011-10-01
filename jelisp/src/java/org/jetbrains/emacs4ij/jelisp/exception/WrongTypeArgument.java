package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/26/11
 * Time: 6:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class WrongTypeArgument extends LispException {
    public WrongTypeArgument (String expectedType, String gotValue) {
        super("Wrong type argument: " + expectedType + " " + gotValue);
    }
}
