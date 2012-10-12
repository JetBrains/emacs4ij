package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/20/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */
public final class LispVector implements LispObject, LispSequence, LispArray, StringOrVector {
    private List<LispObject> myData = null;

    public LispVector() {
        myData = new ArrayList<>();
    }
    
    public LispVector(Shortcut shortcut) {
        throw new NotImplementedException("LispVector.constructor(Shortcut)");
    }

    public LispVector (LispObject ... objects) {
        myData = new ArrayList<>(Arrays.asList(objects));
    }
    
    public LispVector (List<LispObject> list) {
        myData = list; //rem: don't copy for LispSequence.copy to share data
    }

    public LispVector (Collection<LispSymbol> objectArray) {
        myData = new ArrayList<LispObject>(objectArray);
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

    @Nullable
    @Override
    public List<Shortcut> toKeyboardShortcutList() {
        //todo: implement
        GlobalEnvironment.echo(JelispBundle.message("not.supported.with.value", myData.get(0), toString()), GlobalEnvironment.MessageType.WARNING);
        return null;
//        if (myData.get(0).equals(new LispSymbol("menu-bar"))) {
//        GlobalEnvironment.echo(JelispBundle.message("not.supported.with.value", "menu-bars", toString()),
//                    GlobalEnvironment.MessageType.WARNING);
//            return null;
//        }
//        throw new NotImplementedException("LispVector.toKeyboardShortcutList(): " + toString());
    }

    @Override
    public int size() {
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
            data.add(Core.functionCall(environment, method, item));
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
            if (!Predicate.isCharacter(element))
                throw new WrongTypeArgumentException("characterp", element);
            s += ((LispInteger)element).toCharacterString();
        }
        return s;
    }

    public boolean isEmpty() {
        return myData.isEmpty();
    }

    @Override
    public LispVector delete(LispObject element) {
        List<LispObject> data = new ArrayList<>(myData);
        data.remove(element);
        return new LispVector(data);
    }

    /**
     * assume this is an object array, set symbol at first position if no 0-items or at position before first 0
     * @param symbol to set
     */
    public void defineSymbol (LispSymbol symbol) {
        LispInteger zero = new LispInteger(0);
        for (int i = 0; i < myData.size() - 1; ++i) {
            if (myData.get(i + 1).equals(zero)) {
                myData.set(i, symbol);
                return;
            }
        }
        myData.set(0, symbol);
    }
    
    public static LispVector make (int length, LispObject value) {
        ArrayList<LispObject> list = new ArrayList<>(length);
        for (int i = 0; i < length; i++)
            list.add(value);
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

    @Override
    public LispVector substring (int from, int to) {
        return new LispVector(myData.subList(from, to).toArray(new LispObject[to - from]));
    }
}
