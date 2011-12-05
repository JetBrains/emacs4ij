package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/1/11
 * Time: 5:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class Primitive extends LispObject implements FunctionCell {
    private String myName;
    private String myDocumentation;
    private boolean isInteractive;
    private String myInteractiveString;
    private Type myType;

    public enum Type {BUILTIN, SPECIAL_FORM}

    public Primitive (Subroutine annotation, String documentation, Type type) {
        myName = annotation.value();
        isInteractive = annotation.isCmd();
        myInteractiveString = annotation.interactive();
        myDocumentation = documentation;
        myType = type;
    }

    @Override
    public String toString() {
        return "#<subr " + myName + ">";
    }

    /*public String getDocumentation() {
        return myDocumentation;
    }     */

    @Override
    public LispObject getDocString() {

        if (myDocumentation == null)
            return LispSymbol.ourNil;
        /*int i = myDocumentation.indexOf("usage: ");
        if (i == -1)
            return new LispString(myDocumentation);  */
        return new LispString (myDocumentation.replace("usage: (" + myName, "\n(fn"));
    }

    @Override
    public boolean isInteractive() {
        return isInteractive;
    }

    @Override
    public String getInteractiveString() {
        return myInteractiveString;
    }

    public Type getType () {
        return myType;
    }

    @Override
    public LObject evaluate(Environment environment) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
