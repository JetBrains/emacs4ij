package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/6/12
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Sequence {
    private Sequence() {}

    private static boolean isSequence (LispObject object) {
        //Returns t if object is a list, vector, string, char-table, todo: bool-vector, nil otherwise.
        return (object instanceof LispSequence || object.equals(LispSymbol.ourNil));    
    }
    
    @Subroutine("sequencep")
    public static LispSymbol sequenceP (LispObject object) {
        return LispSymbol.bool(isSequence(object));
    }
    
    @Subroutine("length")
    public static LispInteger length (LispObject sequence) {
        if (!isSequence(sequence))
            throw new WrongTypeArgumentException("sequencep", sequence);
        if (sequence.equals(LispSymbol.ourNil))
            return new LispInteger(0);
        return new LispInteger(((LispSequence)sequence).size());
    }

    @Subroutine(value = "append")
    public static LispObject append (@Optional LispObject... args) {
        if (args == null || args.length == 0)
            return LispSymbol.ourNil;
        ArrayList<LispObject> list = new ArrayList<>();
        for (int i = 0; i < args.length - 1; ++i) {
            LispObject sequence = args[i];
            if (!isStringListVectorNil(sequence))
                throw new WrongTypeArgumentException("sequencep", sequence);
            if (sequence.equals(LispSymbol.ourNil))
                continue;
            list.addAll(((LispSequence)sequence).toLispObjectList());
        }
        if (!list.isEmpty()) {
            LispList result = LispList.list(list);
            result.append(args[args.length-1]);
            return result;
        }
        return args[args.length-1];
    }

    public static LispObject verifyFunction(Environment environment, LispObject function) {
        if (function instanceof LispSymbol) {
            function = environment.find(((LispSymbol) function).getName());
            if (!((LispSymbol) function).isFunction())
                throw new VoidFunctionException(((LispSymbol) function).getName());
        } else if (function instanceof LispList) {
            if (!((LispList) function).car().equals(new LispSymbol("lambda")))
                throw new VoidFunctionException(function.toString());
        } else
            throw new InvalidFunctionException(function.toString());
        return function;
    }

    @Subroutine("mapcar")
    public static LispList mapCar (Environment environment, LispObject function, LispObject sequence) {
        if (length(sequence).getData() == 0)
            return LispList.list();
        return LispList.list(((LispSequence)sequence).mapCar(environment, verifyFunction(environment, function)));
    }

    @Subroutine("copy-sequence")
    public static LispObject copySequence (LispObject arg) {
        if (LispSymbol.ourNil.equals(arg))
            return arg;
        //todo: bool-vector
        if (!isSequence(arg))
            throw new WrongTypeArgumentException("sequencep", arg);
        return ((LispSequence)arg).copy();
    }
    
    private static boolean isStringListVectorNil (LispObject object) {
        return (object instanceof LispVector) || (object instanceof LispString) || (object instanceof LispList)
         || (object.equals(LispSymbol.ourNil));
    }
    
    @Subroutine("concat")
    public static LispString concat (@Optional LispObject... sequences) {
        if (sequences == null || sequences.length == 0)
            return new LispString("");
        for (LispObject s: sequences) {
            if (!isStringListVectorNil(s))
                throw new WrongTypeArgumentException("sequencep", s);
        }
        String res = "";
        for (LispObject s: sequences) {
            if (s.equals(LispSymbol.ourNil))
                continue;
            LispSequence sequence = (LispSequence) s;
            res += sequence.toCharString();
        }
        return new LispString(res);
    }
    
    @Subroutine("vconcat")
    public static LispVector vConcat (@Optional LispObject... sequences) {
        if (sequences == null || sequences.length == 0)
            return new LispVector();
        for (LispObject s: sequences) {
            if (!isStringListVectorNil(s))
                throw new WrongTypeArgumentException("sequencep", s);
        }
        LispVector v = new LispVector();
        for (LispObject s : sequences) {
            if (s.equals(LispSymbol.ourNil))
                continue;
            LispSequence sequence = (LispSequence) s;
            v.add(sequence.toLispObjectList());
        }
        return v;
    }

    @Subroutine("mapconcat")
    public static LispString mapConcat (Environment environment, LispObject function, LispObject sequence, LispObject separator) {
        List<LispObject> mapped = mapCar(environment, function, sequence).toLispObjectList();
        if (mapped.isEmpty())
            return new LispString("");
        ArrayList<LispObject> toConcat = new ArrayList<>();
        toConcat.add(mapped.get(0));
        for (int i = 1, mappedSize = mapped.size(); i < mappedSize; i++) {
            LispObject object = mapped.get(i);
            toConcat.add(separator);
            toConcat.add(object);
        }
        return concat(toConcat.toArray(new LispObject[toConcat.size()]));
    }

    @Subroutine("mapc")
    public static LispObject mapc (Environment environment, LispObject function, LispObject sequence) {
        if (length(sequence).getData() == 0)
            return sequence;
        ((LispSequence)sequence).mapCar(environment, verifyFunction(environment, function));
        return sequence;
    }
}
