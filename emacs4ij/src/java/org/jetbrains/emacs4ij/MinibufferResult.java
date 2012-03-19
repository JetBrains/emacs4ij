package org.jetbrains.emacs4ij;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/19/12
 * Time: 12:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class MinibufferResult {
    private boolean toDisplay;
    private LispObject myValue;
    
    public MinibufferResult (LispObject value, boolean display) {
        toDisplay = display;
        myValue = value;
    }

    public boolean toDisplay() {
        return toDisplay;
    }

    public LispObject getValue() {
        return myValue;
    }
}
