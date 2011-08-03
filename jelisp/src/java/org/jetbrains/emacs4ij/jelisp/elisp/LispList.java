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
        if (lispObject == null)
            return;
        myData.add(lispObject);
    }

    public boolean isEmpty() {
        return ((myData == null) || myData.isEmpty());
    }

    @Override
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

    /**
     *
     *
     * @param environment@return the result of last function execution
     */
    @Override
    public LispObject evaluate(Environment environment) {
        if (isEmpty())
            return LispSymbol.ourNil;

        LispSymbol fun;
        try {
            fun = (LispSymbol)car();
        } catch (ClassCastException e) {
            throw new InvalidFunctionException(car().toString());
        }
        LispSymbol symbol = (LispSymbol) environment.find(fun.getName());
        if (symbol == null || symbol.getFunction().equals(LispSymbol.ourVoid))
            throw new VoidFunctionException(fun.getName());

        List<LispObject> data = cdr().getData();

        if (symbol.is(LispSymbol.FunctionType.SpecialForm)) {
            return null;// ((SpecialForm)symbol).execute(environment, data);
        }

        for (int i = 0, dataSize = data.size(); i < dataSize; i++) {
            data.set(i, data.get(i).evaluate(environment));
        }

        if (symbol.is(LispSymbol.FunctionType.BuiltIn))
            return null;// CoreBuiltin.execute(symbol, environment, data);

        return CustomFunction.execute(symbol, environment, data);
    }

    public List<LispObject> getData() {
        return myData;
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
        if (o == null) return false;

        Object list = o;
        if (getClass() != o.getClass()) {
            if (o.getClass() == ArrayList.class) {
                list = new LispList((List<LispObject>)o);
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

    public LispObject car () {
        return ((myData.size() == 0) ? LispSymbol.ourNil : myData.get(0)) ;
    }

    public LispList cdr () {
        return ((myData.size() < 2) ? new LispList() : new LispList(myData.subList(1, myData.size())));
    }

    public LispObject memq (LispObject element) {
        for (int i=0; i!=myData.size(); ++i) {
            if (myData.get(i).equals(element)) {
                return new LispList(myData.subList(i, myData.size()));
            }
        }
        return LispSymbol.ourNil;
    }

}
