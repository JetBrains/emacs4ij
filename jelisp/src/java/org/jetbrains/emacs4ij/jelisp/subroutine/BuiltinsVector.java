package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/28/12
 * Time: 5:16 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsVector {
    private BuiltinsVector() {}

    @Subroutine("make-vector")
    public static LispVector makeVector(LispObject length, LispObject value) {
        if (!BuiltinPredicates.isWholeNumber(length))
            throw new WrongTypeArgumentException("wholenump", length);
        return LispVector.make(((LispInteger)length).getData(), value);
    }

    @Subroutine ("vector")
    public static LispVector vector(@Optional LispObject... args) {
        return new LispVector(args);
    }

}
