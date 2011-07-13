package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp symbol = variable name, function name, constant name, special form name, etc
 */
public class LispSymbol extends LispAtom {
    private String myName = null;

    public LispSymbol(String myName) {
        this.myName = myName;
    }

    @Override
    public String toString() {
        return "LispSymbol{" +
                "myName='" + myName + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispSymbol that = (LispSymbol) o;

        if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return myName != null ? myName.hashCode() : 0;
    }

    @Override
    public LispString toLispString() {
        if (this.equals(Environment.ourNilSymbol))
            return new LispString("nil");
        if (this.equals(Environment.ourTSymbol))
            return new LispString("t");
        return new LispString(myName);
    }
}
