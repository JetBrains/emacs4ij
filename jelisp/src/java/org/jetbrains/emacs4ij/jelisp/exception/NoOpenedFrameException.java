package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 3:59 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoOpenedFrameException extends LispException {
    public NoOpenedFrameException() {
        super(JelispBundle.message("no.opened", "frame"));
    }
}