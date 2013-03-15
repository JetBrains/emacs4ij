package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidHashTableParameterException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/12/12
 * Time: 10:44 AM
 * To change this template use File | Settings | File Templates.
 */
public final class LispHashTable implements LispObject {
    public static enum EqualityMethod {EQL, EQ, EQUAL}

    private HashMap<LispObject, LispObject> myData;
    private final EqualityMethod myEqualityMethod;
    private final LispSymbol myWeakness;
    private int myCapacity;
    private final double myRehashSize;
    private final float myRehashThreshold;

    public LispHashTable (String equalityMethod, int initialCapacity, LispNumber loadFactor, LispSymbol weakness,
                             double rehashSize) {
        try {
            myEqualityMethod = EqualityMethod.valueOf(equalityMethod.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new InvalidHashTableParameterException("test", equalityMethod);
        }

        switch (weakness.getName()) {
            case "nil":case "key":case "value":case "key-or-value":case "key-and-value":case "t":
                myWeakness = weakness;
                break;
            default:
                throw new InvalidHashTableParameterException("weakness", weakness);
        }

        if (initialCapacity < 0)
            throw new InvalidHashTableParameterException("size", initialCapacity);
        if (initialCapacity == 0)
            initialCapacity = 1;

        double dblLoadFactor = loadFactor.getDoubleData();
        if (dblLoadFactor <= 0 || (loadFactor instanceof LispFloat && dblLoadFactor > 1)
                || (loadFactor instanceof LispInteger && dblLoadFactor >= 1))
            throw new InvalidHashTableParameterException("rehash threshold", loadFactor);

        if (rehashSize <= 1)
            throw new InvalidHashTableParameterException("rehash size", (float)rehashSize);

        myRehashThreshold = (float)dblLoadFactor;
        myData = new HashMap<>(initialCapacity, myRehashThreshold);
        myCapacity = initialCapacity;
        myRehashSize = rehashSize;
    }

    protected LispHashTable (EqualityMethod equalityMethod, int initialCapacity, double loadFactor, LispSymbol weakness,
                          double rehashSize) {
        myEqualityMethod = equalityMethod;
        myRehashThreshold = (float)loadFactor;
        myData = new HashMap<>(initialCapacity, myRehashThreshold);
        myCapacity = initialCapacity;
        myRehashSize = rehashSize;
        myWeakness = weakness;
    }

    @Nullable
    public LispObject get (LispObject key) {
        return myData.get(key);
    }

    public void put (LispObject key, LispObject value) {
        while (myCapacity * myRehashThreshold < myData.size()) {
            if (myRehashSize == (int) myRehashSize)
                myCapacity += myRehashSize;
            else myCapacity = (int)(myCapacity * myRehashSize) + 1;
        }

        myData.put(key, value);
    }

    public void remove (LispObject key) {
        if (!myData.containsKey(key))
            return;
        myData.remove(key);
    }

    public void clear() {
        myData.clear();
    }

    public void map (Environment environment, LispObject function) {
        for (Map.Entry<LispObject, LispObject> entry: myData.entrySet()) {
            Core.functionCall(environment, function, entry.getKey(), entry.getValue());
        }
    }

    public LispHashTable copy () {
        LispHashTable copy = new LispHashTable(myEqualityMethod, myCapacity, myRehashThreshold, myWeakness, myRehashSize);
        copy.myData = myData;
        return copy;
    }

    public int size() {
        return myData.size();
    }

    public EqualityMethod getTest() {
        return myEqualityMethod;
    }

    public LispSymbol getWeakness() {
        return myWeakness;
    }

    public double getRehashSize() {
        return myRehashSize;
    }

    public float getRehashThreshold() {
        return myRehashThreshold;
    }

    public int getCapacity() {
        return myCapacity;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("#s(hash-table size ");
        sb.append(myCapacity).append(" test ").append(myEqualityMethod.toString().toLowerCase());
        if (!myWeakness.equals(LispSymbol.NIL))
            sb.append(" weakness ").append(myWeakness.toString());
        sb.append(" rehash-size ").append(myRehashSize)
                .append(" rehash-threshold ").append(myRehashThreshold)
                .append(" data (");
        for (Map.Entry<LispObject, LispObject> entry: myData.entrySet()) {
            sb.append(entry.getKey().toString()).append(" ").append(entry.getValue().toString()).append(" ");
        }
        return sb.toString().trim() + "))";
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LispHashTable)) return false;

        LispHashTable that = (LispHashTable) o;

        if (Double.compare(that.myRehashSize, myRehashSize) != 0) return false;
        if (Float.compare(that.myRehashThreshold, myRehashThreshold) != 0) return false;
        if (myCapacity != that.myCapacity) return false;
        if (myData != null ? !myData.equals(that.myData) : that.myData != null) return false;
        if (myEqualityMethod != that.myEqualityMethod) return false;
        if (myWeakness != null ? !myWeakness.equals(that.myWeakness) : that.myWeakness != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result;
        long temp;
        result = myData != null ? myData.hashCode() : 0;
        result = 31 * result + (myEqualityMethod != null ? myEqualityMethod.hashCode() : 0);
        result = 31 * result + (myWeakness != null ? myWeakness.hashCode() : 0);
        result = 31 * result + myCapacity;
        temp = myRehashSize != +0.0d ? Double.doubleToLongBits(myRehashSize) : 0L;
        result = 31 * result + (int) (temp ^ (temp >>> 32));
        result = 31 * result + (myRehashThreshold != +0.0f ? Float.floatToIntBits(myRehashThreshold) : 0);
        return result;
    }
}
