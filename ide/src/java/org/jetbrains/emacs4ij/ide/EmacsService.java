package org.jetbrains.emacs4ij.ide;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/5/11
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

public abstract class EmacsService  {
    protected String myEmacsParameter;

    public String getEmacsParameter() {
        return myEmacsParameter;
    }

    public void setEmacsParameter(String myEmacsParameter) {
        this.myEmacsParameter = myEmacsParameter;
    }

    protected abstract boolean isParameterSet();

}

