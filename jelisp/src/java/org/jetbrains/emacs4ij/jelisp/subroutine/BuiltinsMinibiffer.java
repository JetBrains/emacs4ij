package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/4/12
 * Time: 3:04 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsMinibiffer {
    private BuiltinsMinibiffer() {}

    @Subroutine("completing-read")
    public static LispList completingRead (LispString prompt, LispObject collection,
                                           @Optional LispSymbol predicate, LispObject requireMatch,
                                           LispString initialInput, LispObject history, LispObject defaultValue,
                                           LispObject inheritInputMethod) {
        System.err.println("CALL completing-read");
        return LispList.list();
    }




}
