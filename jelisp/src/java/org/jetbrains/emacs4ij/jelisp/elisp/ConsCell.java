package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 4:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class ConsCell extends LispObject {
    private LObject myCar;
    private LObject myCdr;

    public ConsCell (LObject car, LObject cdr) {
        myCar = car;
        myCdr = cdr;
    }

    @Override
    public String toString() {
        return "(" + myCar.toString() + " . " + myCdr.toString() + ')';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ConsCell)) return false;

        ConsCell consCell = (ConsCell) o;

        if (myCar != null ? !myCar.equals(consCell.myCar) : consCell.myCar != null) return false;
        if (myCdr != null ? !myCdr.equals(consCell.myCdr) : consCell.myCdr != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myCar != null ? myCar.hashCode() : 0;
        result = 31 * result + (myCdr != null ? myCdr.hashCode() : 0);
        return result;
    }

    @Override
    public LObject evaluate(Environment environment) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
