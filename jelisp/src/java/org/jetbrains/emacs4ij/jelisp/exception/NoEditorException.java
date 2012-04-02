package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/2/12
 * Time: 1:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class NoEditorException extends LispException {
    public NoEditorException () {
        super(JelispBundle.message("no.editor.for.buffer"));
    }
}
