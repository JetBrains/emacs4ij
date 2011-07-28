package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

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
        return lispObject.evaluate(environment);
    }
}
