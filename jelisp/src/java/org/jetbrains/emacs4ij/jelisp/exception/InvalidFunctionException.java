package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/20/11
 * Time: 4:41 PM
 * To change this template use File | Settings | File Templates.
 */
@Error("invalid-function")
public class InvalidFunctionException extends LispException {
    public InvalidFunctionException(String functionName) {
        super("'(invalid-function " + functionName + ')');
    }
}
