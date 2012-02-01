package org.jetbrains.emacs4ij.jelisp.exception;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/12/11
 * Time: 3:07 PM
 * To change this template use File | Settings | File Templates.
 */
public class MissingClosingDoubleQuoteException extends LispException {
    /*public MissingClosingDoubleQuoteException (int position) {
        super("Missing closing double quote", position);
    }*/

    public MissingClosingDoubleQuoteException () {
        super("Missing closing double quote");
    }
}
