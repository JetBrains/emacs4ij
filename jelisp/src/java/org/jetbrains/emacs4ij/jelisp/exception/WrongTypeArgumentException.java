package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/26/11
 * Time: 6:58 PM
 * To change this template use File | Settings | File Templates.
 */

@Error("wrong-type-argument")
public class WrongTypeArgumentException extends LispException {
    public WrongTypeArgumentException(String expectedType, String gotValue) {
        super("'(wrong-type-argument " + expectedType + " " + gotValue + ')');
    }
}
