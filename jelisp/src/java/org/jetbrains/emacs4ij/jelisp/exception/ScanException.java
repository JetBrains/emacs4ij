package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 13/02/12
 * Time: 22:59
 * To change this template use File | Settings | File Templates.
 */

@Error("scan-error")
public class ScanException extends LispException {
    public ScanException (String message) {
        super("'(scan-error " + message + ')');
    }
}
