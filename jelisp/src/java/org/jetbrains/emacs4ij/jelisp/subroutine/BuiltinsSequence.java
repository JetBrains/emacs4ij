package org.jetbrains.emacs4ij.jelisp.subroutine;

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

    @Subroutine("length")
    public static LispInteger length (LObject sequence) {
        if (!BuiltinPredicates.sequenceP(sequence).toBoolean())
            throw new WrongTypeArgumentException("sequencep", sequence.toString());
        if (sequence.equals(LispSymbol.ourNil))
            return new LispInteger(0);
        return new LispInteger(((LispSequence)sequence).length());
    }

    private static boolean isSequence (LObject object) {
        return BuiltinPredicates.sequenceP(object).toBoolean();
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
            if (list.size() == 1)
                System.out.print(1);
            LispList result = LispList.list(list);
            result.append(args[args.length-1]);
            return result;
        }
        return args[args.length-1];
    }

}
