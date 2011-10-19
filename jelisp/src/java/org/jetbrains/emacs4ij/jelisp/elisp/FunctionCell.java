package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/19/11
 * Time: 2:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class FunctionCell extends LispObject {
    public enum Type {BuiltIn, SpecialForm, CustomFunction}
    private String myBody;
    private Type myType;

    public FunctionCell (String body, Type type) {
        myBody = body;
        myType = type;
    }

    @Override
    public LObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public String toString() {
        return myBody;
    }

    public Type getType () {
        return myType;
    }
}
