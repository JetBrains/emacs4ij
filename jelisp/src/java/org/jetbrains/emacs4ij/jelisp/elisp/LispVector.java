package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/20/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispVector extends LispObject {
    private ArrayList<LObject> myData = null;

    public LispVector() {
        myData = new ArrayList<LObject>();
    }

     public LispVector (LObject ... objects) {
        myData = new ArrayList<LObject>(Arrays.asList(objects));
    }

    @Override
    public LObject evaluate(CustomEnvironment environment) {
        return this;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispVector that = (LispVector) o;

        if (myData != null ? !myData.equals(that.myData) : that.myData != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return myData != null ? myData.hashCode() : 0;
    }

    @Override
    public String toString() {
        if (myData.isEmpty())
            return "[]";
        String result = "[";
        for (LObject element: myData) {
            result += element.toString() + ' ';
        }
        return result.trim() + ']';
    }

    public void add (LObject object) {
        if (object == null)
            return;
        myData.add(object);
    }
    
    public LObject get (int index) throws IndexOutOfBoundsException {
        return myData.get(index);
    }
}
