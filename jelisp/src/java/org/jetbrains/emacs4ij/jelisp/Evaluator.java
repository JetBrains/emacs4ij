package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispNumber;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 5:30 PM
 *
 * This is an evaluator for parsed lisp program
 */
public class Evaluator {

    public static LispObject evaluate (LispObject lispObject, Environment environment) {
        if (lispObject instanceof LispNumber)
            return lispObject;
        if (lispObject instanceof LispString)
            return lispObject;
        if (lispObject instanceof LispList) {

        }
        return null;

    }
}
