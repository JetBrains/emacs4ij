package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/20/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispVector implements LispObject, LispSequence, LispArray, LispStringOrVector {
    private List<LispObject> myData = null;

    public LispVector() {
        myData = new ArrayList<>();
    }

    public LispVector (LispObject ... objects) {
        myData = new ArrayList<>(Arrays.asList(objects));
    }
    
    public LispVector (List<LispObject> list) {
        myData = list; //rem: don't copy for LispSequence.copy to share data 
        //myData = Lists.newArrayList(list);
    }

    @Override
    public LispObject evaluate(Environment environment) {
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
        for (LispObject element: myData) {
            result += element.toString() + ' ';
        }
        return result.trim() + ']';
    }

    public void add (LispObject object) {
        if (object == null)
            return;
        myData.add(object);
    }
    
    public void add (List<LispObject> objects) {
        if (objects == null || objects.isEmpty())
            return;
        myData.addAll(objects);
    }

    public LispObject get (int index) throws IndexOutOfBoundsException {
        return myData.get(index);
    }

    @Override
    public int length() {
        return myData.size();
    }

    @Override
    public List<LispObject> toLispObjectList() {
        return myData;
    }

    @Override
    public List<LispObject> mapCar(Environment environment, LispObject method) {
        ArrayList<LispObject> data = new ArrayList<>();
        for (LispObject item: toLispObjectList()) {
            data.add(BuiltinsCore.functionCall(environment, method, item));
        }
        return data;
    }

    @Override
    public LispObject copy() {
        return new LispVector(myData);
    }

    @Override
    public String toCharString() {
        String s = "";
        for (LispObject element: myData) {
            if (!BuiltinPredicates.isCharacter(element))
                throw new WrongTypeArgumentException("characterp", element);
            s += ((LispInteger)element).toCharacterString();
        }
        return s;
    }

    public boolean isEmpty() {
        return myData.isEmpty();
    }
    
    public void setFirst (LispObject first) {
        myData.set(0, first);
    }
    
    public static LispVector make (int length, LispObject value) {
        ArrayList<LispObject> list = new ArrayList<>(length);
        Collections.fill(list, value);
        return new LispVector(list);
    }

    @Override
    public void setItem(int position, LispObject value) {
        myData.set(position, value);
    }

    @Override
    public LispObject getItem(int position) {
        return myData.get(position);
    }
    
    public LispVector subString (int from, int to) {
        return new LispVector(myData.subList(from, to).toArray(new LispObject[to - from]));
    }

    @Override
    public String toShortcutString() {
        throw new NotImplementedException("LispVector.toShortcutString()");
    }
}
