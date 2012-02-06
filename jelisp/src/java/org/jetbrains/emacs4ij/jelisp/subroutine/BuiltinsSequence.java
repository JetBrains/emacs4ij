package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSequence;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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
}
