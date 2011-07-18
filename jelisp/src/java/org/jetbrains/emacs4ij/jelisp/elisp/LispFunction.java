package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 12:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class LispFunction extends LispObject {
    protected LispSymbol myName = null;
    public abstract LispObject execute (List<LispObject> args, Environment environment);

    public LispSymbol getName () {
        return myName;
    }

}
