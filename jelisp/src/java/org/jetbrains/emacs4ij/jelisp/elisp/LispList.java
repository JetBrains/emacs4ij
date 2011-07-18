package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.ArrayList;
import java.util.Arrays;
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
    private List<LispObject> myData = null;

    public LispList() {
        myData = new ArrayList<LispObject>();
    }

    public LispList (LispObject ... objects) {
        myData = Arrays.asList(objects);
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

    public List<LispObject> getData() {
        return myData;
    }

    public LispObject get (int index) {
        return myData.get(index);
    }

    public void set (int index, LispObject newValue) {
        myData.set(index, newValue);
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

        return !(myData != null ? !myData.equals(lispList.myData) : lispList.myData != null);

    }

    @Override
    public int hashCode() {
        return myData != null ? myData.hashCode() : 0;
    }

    public LispObject car () {
        return ((myData.size() == 0) ? LispSymbol.ourNilSymbol : myData.get(0)) ;
    }

    public LispObject cdr () {
        return ((myData.size() < 2) ? LispSymbol.ourNilSymbol : new LispList(myData.subList(1, myData.size())));
    }
}
