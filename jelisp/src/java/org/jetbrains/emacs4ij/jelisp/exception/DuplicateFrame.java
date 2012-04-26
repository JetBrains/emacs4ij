package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/26/12
 * Time: 8:11 PM
 * To change this template use File | Settings | File Templates.
 */
public class DuplicateFrame extends LispException {
    public DuplicateFrame (String frame) {
        super(JelispBundle.message("double.frame", frame));
    }
}
