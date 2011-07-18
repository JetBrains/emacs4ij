package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 10:31 AM
 * To change this template use File | Settings | File Templates.
 */
public class VoidVariableException extends LispException {
    public VoidVariableException (String varName) {
        super("Void variable " + varName);
    }
}
