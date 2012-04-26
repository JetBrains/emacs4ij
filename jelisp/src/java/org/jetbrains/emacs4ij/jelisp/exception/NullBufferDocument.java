package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class NullBufferDocument extends LispException {
    public NullBufferDocument (String bufferName) {
        super(JelispBundle.message("no.doc", bufferName));
    }
}
