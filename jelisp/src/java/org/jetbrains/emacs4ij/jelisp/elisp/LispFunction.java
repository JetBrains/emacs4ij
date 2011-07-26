package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.Arrays;
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

    public LispFunction (String name) {
        myName = new LispSymbol(name);
    }

    public LispFunction() {
    }

    public abstract LispObject execute (Environment environment, List<LispObject> args);

    @Override
    /**
     * takes Environment and List<LispObject> args
     */
    public LispObject evaluate (Object... parameters) {
        List<LispObject> args;
        Environment environment;
        try {
            //TODO: wtf??
            environment =(Environment) Arrays.asList(parameters).get(0);
            args = (List<LispObject>) Arrays.asList(parameters).get(1);

        } catch (ClassCastException e) {
            throw new RuntimeException("invalid function evaluation arguments!");
        }
        return execute(environment, args);
    }

    public LispSymbol getName () {
        return myName;
    }

}
