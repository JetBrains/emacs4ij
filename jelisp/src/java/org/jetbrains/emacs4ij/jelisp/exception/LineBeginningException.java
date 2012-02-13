package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 4:28 PM
 * To change this template use File | Settings | File Templates.
 */
public class LineBeginningException extends LispException {
    public LineBeginningException () {
        super("Reached the beginning of line");
    }
}
