package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/1/11
 * Time: 4:32 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class NamedLispObject extends LispObject {
    protected LispSymbol myName = null;

    public LispSymbol getName() {
        return myName;
    }

    public LispObject getProperty (LispSymbol propertyName) {
        return myName.getProperty(propertyName);
    }

    public void setProperty (LispSymbol propertyName, LispObject value) {
        myName.setProperty(propertyName, value);
    }
}
