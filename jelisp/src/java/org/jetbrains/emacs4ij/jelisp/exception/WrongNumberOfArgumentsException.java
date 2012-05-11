package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */
@Error("wrong-number-of-arguments")
public class WrongNumberOfArgumentsException extends LispException {
    public WrongNumberOfArgumentsException(String functionName, int nArgs) {
        super("'(wrong-number-of-arguments " + functionName + " " + nArgs + ')');
    }

    public WrongNumberOfArgumentsException(String functionName) {
        super("'(wrong-number-of-arguments " + functionName + ')');
    }
}
