package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/12/11
 * Time: 3:35 PM
 * To change this template use File | Settings | File Templates.
 */
public class UnknownCodeBlockException extends LispException {
    public UnknownCodeBlockException(String block) {
        super("Unknown code block: " + block);
    }
}
