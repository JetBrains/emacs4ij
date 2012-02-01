package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 6:59 PM
 * To change this template use File | Settings | File Templates.
 */
public class ReadFinishedException extends LispException {
    public ReadFinishedException () {
        super("The input Reader has reached its end");
    }
}
