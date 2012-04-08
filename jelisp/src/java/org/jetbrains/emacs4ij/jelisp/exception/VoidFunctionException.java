package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/21/11
 * Time: 1:23 PM
 * To change this template use File | Settings | File Templates.
 */
@Error("void-function")
public class VoidFunctionException extends LispException {
    public VoidFunctionException (String functionName) {
        super("'(void-function " + functionName + ')');
    }
}
