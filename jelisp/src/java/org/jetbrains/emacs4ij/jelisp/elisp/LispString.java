package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:16 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp string = "anything between double quotation marks"
 */
public class LispString extends LispAtom {
    private String myData;

    public LispString (String data) {
        myData = data.replaceAll("\\\\\"", "\"");
    }

    @Override
    public String toString() {
        return myData;
        /*return "LispString{" +
                "myData='" + myData + '\'' +
                '}';*/
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispString that = (LispString) o;

        return !(myData != null ? !myData.equals(that.myData) : that.myData != null);

    }

    @Override
    public int hashCode() {
        return myData != null ? myData.hashCode() : 0;
    }

    @Override
    public LispString toLispString() {
        return new LispString('"' + myData + '"');
    }

    @Override
    /**
     * no parameters required
     */
    public LObject evaluate(Environment environment) {
        return this;
    }
}
