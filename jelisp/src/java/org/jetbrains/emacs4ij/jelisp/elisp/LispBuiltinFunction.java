package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispBuiltinFunction extends LispObject{

    private String myName;

    public String getName() {
        return myName;
    }

    @Override
    public LispString toLispString() {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
