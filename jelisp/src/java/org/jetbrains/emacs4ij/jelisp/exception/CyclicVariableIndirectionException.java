package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/12/12
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */

@Error("cyclic-variable-indirection")
public class CyclicVariableIndirectionException extends LispException {
    public CyclicVariableIndirectionException (LispSymbol var) {
        super(JelispBundle.message("cyclic.var.indirection", var.getName()));
    }
}
