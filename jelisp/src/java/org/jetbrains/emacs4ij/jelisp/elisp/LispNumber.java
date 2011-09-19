package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:17 PM
 * To change this template use File | Settings | File Templates.
 *
 * base class for elisp numbers
 */
public abstract class LispNumber extends LispAtom {
    @Override
    /**
     * no parameters required
     */
    public LispObject evaluate(Environment environment) {
        return this;
    }

}
