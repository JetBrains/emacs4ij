package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 1:30 PM
 * To change this template use File | Settings | File Templates.
 *
 * this class is a lisp list = (something in brackets 5 5 delimited by spaces or line breaks)
 */
public class LispList extends LispObject {
    private ArrayList<LispObject> myData = null;

    public LispList() {
        myData = new ArrayList<LispObject>();
    }

    public LispList (List<LispObject> data) {
        myData = new ArrayList<LispObject>(data);
    }

    public void add (LispObject lispObject) {
        myData.add(lispObject);
    }

    public boolean isEmpty() {
        return ((myData == null) || myData.isEmpty());
    }

    public LispString toLispString() {
        String list = "(";
        if (isEmpty()) {
            list += ")";
            return new LispString(list);
        }
        for (LispObject lispObject: myData) {
            list += lispObject.toLispString() + " ";
        }
        list = list.trim();
        list += ")";
        return new LispString(list);
    }

    @Override
    public String toString() {
        return "LispList{" +
                "myData=" + myData +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispList lispList = (LispList) o;

        if (myData != null ? !myData.equals(lispList.myData) : lispList.myData != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return myData != null ? myData.hashCode() : 0;
    }
}
