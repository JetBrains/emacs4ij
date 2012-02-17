package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/6/12
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsSequence {
    private BuiltinsSequence() {}

    private static boolean isSequence (LObject object) {
        //Returns t if object is a list, vector, string, todo: bool-vector, or char-table, nil otherwise. 
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
            if (!isSequence(sequence))
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
    public static LObject mapCar (Environment environment, LispSymbol function, LObject sequence) {
        //todo: char-table may not fit as sequence here
        int length = length(sequence).getData();
        if (length == 0)
            return LispSymbol.ourNil;
        return LispList.list(((LispSequence)sequence).mapCar(environment, function));
    }

}
