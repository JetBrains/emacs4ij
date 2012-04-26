package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoWindowForBuffer extends LispException {
    public NoWindowForBuffer (String bufferName) {
        super(JelispBundle.message("no.window.for.buffer", bufferName));
    }
}
