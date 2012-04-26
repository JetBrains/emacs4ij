package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 1:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class AssignNullDocument extends LispException {
    public AssignNullDocument(String bufferName) {
        super(JelispBundle.message("null.doc", bufferName));
    }
}
