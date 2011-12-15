package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 7:21 PM
 * To change this template use File | Settings | File Templates.
 */

@Error("args-out-of-range")
public class ArgumentOutOfRange extends LispException {
    public ArgumentOutOfRange (String argument, String value) {
        super("'(args-out-of-range " + argument + ' ' + value + ')');
    }
}
