package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 12:12 PM
 * To change this template use File | Settings | File Templates.
 */

@Error("cyclic-function-indirection")
public class CyclicFunctionIndirectionException extends LispException {
    public CyclicFunctionIndirectionException (String message) {
        super("'(cyclic-function-indirection " + message + ')');
    }
}
