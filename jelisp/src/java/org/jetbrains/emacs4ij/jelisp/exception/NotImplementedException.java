package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/13/12
 * Time: 8:43 AM
 * To change this template use File | Settings | File Templates.
 */
public class NotImplementedException extends LispException {
    public NotImplementedException (String what) {
        super("Not implemented: " + what);
    }
}
