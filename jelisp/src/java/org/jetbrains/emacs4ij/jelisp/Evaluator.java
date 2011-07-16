package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;

import java.util.ArrayList;

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
            LispSymbol fun = (LispSymbol)((LispList)lispObject).car();
            LispObject lispObject1 = environment.find(fun.getMyPrintName());


            if (lispObject1 instanceof LispBuiltinFunction) {
                ArrayList<LispObject> data = ((LispList) lispObject).cdr().getData();
                for (int i = 0, dataSize = data.size(); i < dataSize; i++) {
                    data.set(i, evaluate(data.get(i), environment));
                }
                return ((LispBuiltinFunction) lispObject1).execute(data);
            }

        }
        return null;

    }
}
