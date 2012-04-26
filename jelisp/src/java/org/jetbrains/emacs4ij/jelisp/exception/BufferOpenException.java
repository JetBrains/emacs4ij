package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 1:11 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferOpenException extends LispException {
    public BufferOpenException (String bufferName) {
        super(JelispBundle.message("open.buffer.with.other.doc", bufferName));
    }
}
