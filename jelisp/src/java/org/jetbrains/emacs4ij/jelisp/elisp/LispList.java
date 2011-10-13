package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;

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
    private ArrayList<LObject> myData = null;

    public LispList() {
        myData = new ArrayList<LObject>();
    }

    public LispList (LObject ... objects) {
        myData = new ArrayList<LObject>(Arrays.asList(objects));
    }

    public LispList (List<LObject> data) {
        myData = new ArrayList<LObject>(data);
    }

    public void add (LObject lispObject) {
        if (lispObject == null)
            return;
        myData.add(lispObject);
    }

    public boolean isEmpty() {
        return ((myData == null) || myData.isEmpty());
    }

    /**
     *
     *
     * @param environment@return the result of last function execution
     */
    @Override
    public LObject evaluate(Environment environment) {
        if (isEmpty())
            return LispSymbol.ourNil;

        LispSymbol fun;
        try {
            fun = (LispSymbol)car();
        } catch (ClassCastException e) {
            throw new InvalidFunctionException(car().toString());
        }
        LispSymbol symbol = environment.getMainEnvironment().find(fun.getName());
        if (symbol == null || symbol.getFunction().equals(LispSymbol.ourVoid))
            throw new VoidFunctionException(fun.getName());

        List<LObject> data = cdr().getData();

        return symbol.evaluateFunction(environment, data);
    }

    public List<LObject> getData() {
        return myData;
    }

    @Override
    public String toString() {
        if (isEmpty())
            return "()";
        String list = "(";
        for (LObject lispObject: myData) {
            list += lispObject.toString() + " ";
        }
        return list.trim() + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;

        Object list = o;
        if (getClass() != o.getClass()) {
            if (o.getClass() == ArrayList.class) {
                list = new LispList((List<LObject>)o);
            } else {
                return false;
            }
        }

        LispList lispList = (LispList) list;
        return !(myData != null ? !myData.equals(lispList.myData) : lispList.myData != null);

    }

    @Override
    public int hashCode() {
        return myData != null ? myData.hashCode() : 0;
    }

    public LObject car () {
        return ((myData.size() == 0) ? LispSymbol.ourNil : myData.get(0)) ;
    }

    public LispList cdr () {
        return ((myData.size() < 2) ? new LispList() : new LispList(myData.subList(1, myData.size())));
    }

    public LispObject memq (LObject element) {
        for (int i=0; i!=myData.size(); ++i) {
            if (myData.get(i).equals(element)) {
                return new LispList(myData.subList(i, myData.size()));
            }
        }
        return LispSymbol.ourNil;
    }

}
