package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */
public class WrongNumberOfArgumentsException extends LispException {
    public WrongNumberOfArgumentsException(String functionName) {
        super(functionName +  ": Wrong number of arguments");
    }
}
