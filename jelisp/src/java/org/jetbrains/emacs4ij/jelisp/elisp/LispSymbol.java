package org.jetbrains.emacs4ij.jelisp.elisp;

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
    public static final LispSymbol ourNilSymbol = new LispSymbol("nil");
    public static final LispSymbol ourTSymbol = new LispSymbol("t");

    private String myPrintName = null;
    private LispObject myValue = null;
    //myFunction
    //myPropertyList

    public LispSymbol(String myName) {
        this.myPrintName = myName;
    }

    public String getMyPrintName() {
        return myPrintName;
    }

    @Override
    public String toString() {
        return "LispSymbol{" +
                "myPrintName='" + myPrintName + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispSymbol that = (LispSymbol) o;

        return !(myPrintName != null ? !myPrintName.equals(that.myPrintName) : that.myPrintName != null);

    }

    @Override
    public int hashCode() {
        return myPrintName != null ? myPrintName.hashCode() : 0;
    }

    @Override
    public LispString toLispString() {
        if (this.equals(ourNilSymbol))
            return new LispString("nil");
        if (this.equals(ourTSymbol))
            return new LispString("t");
        return new LispString(myPrintName);
    }
}
