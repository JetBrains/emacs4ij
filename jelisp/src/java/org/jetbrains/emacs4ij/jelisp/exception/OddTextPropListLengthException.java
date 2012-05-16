package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/16/12
 * Time: 11:20 AM
 * To change this template use File | Settings | File Templates.
 */

public class OddTextPropListLengthException extends LispException {
    public OddTextPropListLengthException() {
        super(JelispBundle.message("odd.length.property.list"));
    }
}
