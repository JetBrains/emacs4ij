package org.jetbrains.emacs4ij.jelisp.parser.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 13/02/12
 * Time: 22:59
 * To change this template use File | Settings | File Templates.
 */

@org.jetbrains.emacs4ij.jelisp.exception.Error("scan-error")
public class ScanException extends ParserException {
    public ScanException (String message) {
        super("'(scan-error " + message + ')');
    }

    public ScanException (String message, int lastOkPosition, int from) {
        super("'(scan-error " + message + " " + lastOkPosition + " " + from + ")");
    }
}
