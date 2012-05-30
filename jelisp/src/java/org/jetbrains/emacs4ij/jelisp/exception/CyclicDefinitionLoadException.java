package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/30/12
 * Time: 10:19 AM
 * To change this template use File | Settings | File Templates.
 */
public class CyclicDefinitionLoadException extends LispException {
    public CyclicDefinitionLoadException (String id) {
        super(JelispBundle.message("cyclic.def.load", id));
    }
}
