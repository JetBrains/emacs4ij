package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/12/12
 * Time: 10:55 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class HashTable {
    private HashTable() {}

    @Subroutine("make-hash-table")
    public static LispHashTable makeHashTable (@Optional LispObject... keywordArguments) {
        String equalityMethod = null;
        LispSymbol weakness = null;
        int initialCapacity = -1;
        double rehashSize = -1;
        double rehashThreshold = -1;

        for (int i = 0; i < keywordArguments.length; i += 2) {
            LispObject keyword = keywordArguments[i];
            if (!(keyword instanceof LispSymbol) || !((LispSymbol) keyword).isKeyword() || (i + 1 >= keywordArguments.length))
                throw new InvalidArgumentListException(keyword);
            LispObject value = keywordArguments[i + 1];
            switch (((LispSymbol) keyword).getName()) {
                case ":test":
                    if (equalityMethod != null)
                        throw new InvalidArgumentListException(keyword);
                    if (!(value instanceof LispSymbol))
                        throw new WrongTypeArgumentException("symbolp", value);
                    equalityMethod = ((LispSymbol) value).getName();
                    break;
                case ":weakness":
                    if (weakness != null)
                        throw new InvalidArgumentListException(keyword);
                    if (!(value instanceof LispSymbol))
                        throw new InvalidHashTableParameterException("weakness", value);
                    weakness = (LispSymbol) value;
                    break;
                case ":size":
                    if (initialCapacity != -1)
                        throw new InvalidArgumentListException(keyword);
                    if (!Predicate.isInteger(value))
                        throw new InvalidHashTableParameterException("size", value);
                    initialCapacity = ((LispInteger) value).getData();
                    break;
                case ":rehash-size":
                    if (rehashSize != -1)
                        throw new InvalidArgumentListException(keyword);
                    if (!Predicate.isNumber(value))
                        throw new InvalidHashTableParameterException("rehash size", value);
                    rehashSize = ((LispNumber) value).getDoubleData();
                    break;
                case ":rehash-threshold":
                    if (rehashThreshold != -1)
                        throw new InvalidArgumentListException(keyword);
                    if (!Predicate.isNumber(value))
                        throw new InvalidHashTableParameterException("rehash threshold", value);
                    rehashThreshold = ((LispNumber) value).getDoubleData();
                    break;
                default:
                    throw new InvalidArgumentListException(keyword);
            }
        }

        if (equalityMethod == null)
            equalityMethod = "eql";
        if (weakness == null)
            weakness = LispSymbol.ourNil;
        if (initialCapacity == -1)
            initialCapacity = 65;
        if (rehashSize == -1)
            rehashSize = 1.5;
        if (rehashThreshold == -1)
            rehashThreshold = 0.8;

        return new LispHashTable(equalityMethod, initialCapacity, rehashThreshold, weakness, rehashSize);
    }

    @Subroutine("gethash")
    public static LispObject getHash (LispObject key, LispHashTable table, @Optional LispObject defaultValue) {
        if (Predicate.isNil(defaultValue))
            defaultValue = LispSymbol.ourNil;
        LispObject value = table.get(key);
        return value == null ? defaultValue : value;
    }

    @Subroutine("puthash")
    public static LispObject putHash (LispObject key, LispObject value, LispHashTable table) {
        table.put(key, value);
        return value;
    }

    @Subroutine("remhash")
    public static LispSymbol removeHash (LispObject key, LispHashTable table) {
        table.remove(key);
        return LispSymbol.ourNil;
    }

    @Subroutine("clrhash")
    public static LispSymbol removeHash (LispHashTable table) {
        table.clear();
        return LispSymbol.ourNil;
    }

    @Subroutine("maphash")
    public static LispSymbol mapHash (Environment environment, LispObject function, LispHashTable table) {
        if (function instanceof LispSymbol) {
            function = environment.find(((LispSymbol) function).getName());
            if (!((LispSymbol) function).isFunction())
                throw new VoidFunctionException(((LispSymbol) function).getName());
        } else if (function instanceof LispList) {
            if (!((LispList) function).car().equals(new LispSymbol("lambda")))
                throw new VoidFunctionException(function.toString());
        } else
            throw new InvalidFunctionException(function.toString());

        table.map(environment, function);
        return LispSymbol.ourNil;
    }

    @Subroutine("define-hash-table-test")
    public static void defineHashTableTest (LispObject name, LispObject testFunction, LispObject hashFunction) {
        throw new UnsupportedOperationException("FUN define-hash-table-test");
    }

    @Subroutine("sxhash")
    public static LispInteger hashCode (LispObject object) {
        return new LispInteger(object.hashCode());
    }

    @Subroutine("copy-hash-table")
    public static LispHashTable copyHashTable (LispHashTable table) {
        return table.copy();
    }

    @Subroutine("hash-table-count")
    public static LispInteger hashTableCount (LispHashTable table) {
        return new LispInteger(table.size());
    }

    @Subroutine("hash-table-test")
    public static LispSymbol hashTableTest (LispHashTable table) {
        return new LispSymbol(table.getTest().toString().toLowerCase());
    }

    @Subroutine("hash-table-weakness")
    public static LispSymbol hashTableWeakness (LispHashTable table) {
        return table.getWeakness();
    }

    @Subroutine("hash-table-rehash-size")
    public static LispNumber hashTableRehashSize (LispHashTable table) {
        double rehashSize = table.getRehashSize();
        return rehashSize == Math.round(rehashSize) ? new LispInteger((int) rehashSize) : new LispFloat(rehashSize);
    }

    @Subroutine("hash-table-rehash-threshold")
    public static LispFloat hashTableRehashThreshold (LispHashTable table) {
        return new LispFloat(table.getRehashThreshold());
    }

    @Subroutine("hash-table-size")
    public static LispInteger hashTableSize (LispHashTable table) {
        return new LispInteger(table.getCapacity());
    }
}
