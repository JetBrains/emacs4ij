package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/4/12
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class SetConstException extends LispException {
    public SetConstException (String name) {
        super("'(" + JelispBundle.message("set.const", name) + ')');
    }
}
