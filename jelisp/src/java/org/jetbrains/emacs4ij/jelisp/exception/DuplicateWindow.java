package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispWindow;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 6:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class DuplicateWindow extends LispException {
    public DuplicateWindow (LispWindow window) {
        super(JelispBundle.message("double.window", window.toString()));
    }
}
