package org.jetbrains.emacs4ij.jelisp.elisp;

import com.rits.cloning.Cloner;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/21/12
 * Time: 12:00 PM
 * To change this template use File | Settings | File Templates.
 */
public class GenericCharTable implements LispCharTable {
    public static final int MAX_N_EXTRA_SLOTS = 10;
    private Map<Integer, LispObject> myData = new HashMap<>();
    private LispObject myDefault;
    private LispObject myParent;
    private LispSymbol mySubtype;
    private LispObject[] myExtras = new LispObject[MAX_N_EXTRA_SLOTS];
    private int myNExtras = 0;

    public GenericCharTable(LispSymbol purpose, @Nullable LispObject init) {
        LispObject n = purpose.getProperty("char-table-extra-slots");
        if (!Predicate.isNil(n)) {
            if (!Predicate.isWholeNumber(n))
                throw new WrongTypeArgumentException("wholenump", n);
            myNExtras = ((LispInteger)n).getData();
            if (myNExtras > MAX_N_EXTRA_SLOTS)
                throw new ArgumentOutOfRange(n, LispSymbol.ourNil);
        }
        myParent = LispSymbol.ourNil;
        myDefault = init == null ? LispSymbol.ourNil : init;
        mySubtype = purpose;
        Arrays.fill(myExtras, myDefault);
    }

    public LispSymbol getSubtype() {
        return mySubtype;
    }

    public void setParent(LispObject parent) {
        myParent = parent;
    }

    public LispObject getParent() {
        return myParent;
    }

    public void setDefault(LispObject myDefault) {
        this.myDefault = myDefault;
    }

    public LispObject getDefault() {
        return myDefault;
    }

    @Override
    public String toString() {
        String s = "#^[" + myDefault + ' ' + myParent + ' ' + mySubtype;
        for (LispObject item: myData.values()) {
            s += ' ' + item.toString();
        }
        for (int i = 0; i < myNExtras; ++i) {
            s += ' ' + myExtras[i].toString();
        }
        s += ']';
        return s;
    }

    public LispObject getExtraSlot(int n) {
        return myExtras[n];
    }

    public void setExtraSlot (int n, LispObject value) {
        myExtras[n] = value;
    }

    public void setContent (LispObject value) {
        for (Integer key: myData.keySet()) {
            myData.put(key, value);
        }
    }

    @Override
    public void setItem(int c, LispObject value) {
//        if (!myData.containsKey(c))
//            throw new InternalException();
        myData.put(c, value);
    }

    public void setRange (int from, int to, LispObject value) {
        for (int i = from; i <= to;  i++) {
            setItem(i, value);
        }
    }

    public LispObject ref(int c) {
        LispObject value = myData.get(c);
        if (value == null) {
            value = myDefault;
            if (Predicate.isNil(value) && myParent instanceof GenericCharTable)
                value = ((GenericCharTable) myParent).ref(c);
        }
        return value;
    }

    public LispObject refAndRange (int c, int from, int to) {
        LispObject val = myData.get(c);
        if (Predicate.isNil(val))
            val = myDefault;
        return val;
    }

    @Override
    public LispObject getItem(int position) {
        if (!myData.containsKey(position))
            return myDefault;
        return myData.get(position);
    }

    @Override
    public int size() {
        return myData.size();
    }

    @Override
    public LispObject copy() {
        Cloner c = new Cloner();
        return c.deepClone(this);
    }

    @Override
    public boolean isEmpty() {
        return myData.isEmpty();
    }

    //=========== unsupported ===========================================================

    @Override
    public LispObject delete(LispObject element) {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<LispObject> toLispObjectList() {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<LispObject> mapCar(Environment environment, LispObject method) {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toCharString() {
        throw new UnsupportedOperationException();
    }

    @Override
    public LispObject evaluate(Environment environment) {
        throw new UnsupportedOperationException();
    }
}
