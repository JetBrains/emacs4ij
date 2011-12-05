package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/12/11
 * Time: 4:44 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp integer number = 13, 1355, -7979, etc
 */
public class LispInteger extends LispNumber<Integer> {
    //private int myData;

    public LispInteger(int data) {
        myData = data;
    }

    @Override
    public String toString() {
        return Integer.toString(myData);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispInteger that = (LispInteger) o;

        return myData.equals(that.myData);
    }

    @Override
    public int hashCode() {
        return myData;
    }

}
