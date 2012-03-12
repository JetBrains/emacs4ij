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
public abstract class BuiltinsSequence {
    private BuiltinsSequence() {}

    private static boolean isSequence (LObject object) {
        //Returns t if object is a list, vector, string, char-table, todo: bool-vector, nil otherwise.
        return (object instanceof LispSequence || object.equals(LispSymbol.ourNil));    
    }
    
    @Subroutine("sequencep")
    public static LispSymbol sequenceP (LObject object) {
        return LispSymbol.bool(isSequence(object));
    }
    
    @Subroutine("length")
    public static LispInteger length (LObject sequence) {
        if (!isSequence(sequence))
            throw new WrongTypeArgumentException("sequencep", sequence.toString());
        if (sequence.equals(LispSymbol.ourNil))
            return new LispInteger(0);
        return new LispInteger(((LispSequence)sequence).length());
    }

    @Subroutine(value = "append")
    public static LObject append (@Optional LObject... args) {
        if (args == null || args.length == 0)
            return LispSymbol.ourNil;
        ArrayList<LObject> list = new ArrayList<>();
        for (int i = 0; i < args.length - 1; ++i) {
            LObject sequence = args[i];
            if (!isStringListVectorNil(sequence))
                throw new WrongTypeArgumentException("sequencep", sequence.toString());
            if (sequence.equals(LispSymbol.ourNil))
                continue;
            list.addAll(((LispSequence)sequence).toLObjectList());
        }
        if (!list.isEmpty()) {
            LispList result = LispList.list(list);
            result.append(args[args.length-1]);
            return result;
        }
        return args[args.length-1];
    }
    
    @Subroutine("mapcar")
    public static LispList mapCar (Environment environment, LObject function, LObject sequence) {
        int length = length(sequence).getData();
        if (sequence instanceof LispCharTable)
            throw new WrongTypeArgumentException("listp", sequence.toString());
        if (length == 0)
            return LispList.list();
        if (function instanceof LispSymbol) {
            function = environment.find(((LispSymbol) function).getName());
            if (!((LispSymbol) function).isFunction())
                throw new VoidFunctionException(((LispSymbol) function).getName());
        } else if (function instanceof LispList) {
            if (!((LispList) function).car().equals(new LispSymbol("lambda")))
                throw new VoidFunctionException(function.toString());
        } else
            throw new InvalidFunctionException(function.toString());
        return LispList.list(((LispSequence)sequence).mapCar(environment, function));
    }

    @Subroutine("copy-sequence")
    public static LObject copySequence (LObject arg) {
        if (LispSymbol.ourNil.equals(arg))
            return arg;
        //todo: bool-vector
        if (!isSequence(arg))
            throw new WrongTypeArgumentException("sequencep", arg.toString());
        return ((LispSequence)arg).copy();
    }
    
    private static boolean isStringListVectorNil (LObject object) {
        return (object instanceof LispVector) || (object instanceof LispString) || (object instanceof LispList)
         || (object.equals(LispSymbol.ourNil));
    }
    
    @Subroutine("concat")
    public static LispString concat (Environment environment, @Optional LObject... sequences) {
        if (sequences == null || sequences.length == 0)
            return new LispString("");
        for (LObject s: sequences) {
            if (!isStringListVectorNil(s))
                throw new WrongTypeArgumentException("sequencep", s.toString()); 
        }
        String res = "";
        for (LObject s: sequences) {
            if (s.equals(LispSymbol.ourNil))
                continue;
            LispSequence sequence = (LispSequence) s;
            res += sequence.toCharString();
        }
        return new LispString(res);
    }
    
    @Subroutine("vconcat")
    public static LispVector vConcat (Environment environment, @Optional LObject... sequences) {
        if (sequences == null || sequences.length == 0)
            return new LispVector();
        for (LObject s: sequences) {
            if (!isStringListVectorNil(s))
                throw new WrongTypeArgumentException("sequencep", s.toString());
        }
        LispVector v = new LispVector();
        for (LObject s : sequences) {
            if (s.equals(LispSymbol.ourNil))
                continue;
            LispSequence sequence = (LispSequence) s;
            v.add(sequence.toLObjectList());
        }
        return v;
    }

    @Subroutine("mapconcat")
    public static LispString mapConcat (Environment environment, LObject function, LObject sequence, LObject separator) {
        List<LObject> mapped = mapCar(environment, function, sequence).toLObjectList();
        if (mapped.isEmpty())
            return new LispString("");
        ArrayList<LObject> toConcat = new ArrayList<>();
        toConcat.add(mapped.get(0));
        for (int i = 1, mappedSize = mapped.size(); i < mappedSize; i++) {
            LObject object = mapped.get(i);
            toConcat.add(separator);
            toConcat.add(object);
        }
        return concat(environment, toConcat.toArray(new LObject[toConcat.size()]));
    }

}
