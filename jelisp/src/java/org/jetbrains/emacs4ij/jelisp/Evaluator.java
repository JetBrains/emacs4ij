package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;

import java.util.List;

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

            if (((LispList) lispObject).isEmpty())
                return LispSymbol.ourNilSymbol;

            LispSymbol fun;
            try {
                fun = (LispSymbol)((LispList)lispObject).car();
            } catch (ClassCastException e) {
                throw new InvalidFunctionException(((LispList)lispObject).car().toString());
            }
            LispObject lispObject1 = environment.find(fun.getName());

            if (lispObject1 instanceof LispFunction) {
                List<LispObject> data = ((LispList)((LispList) lispObject).cdr()).getData();
                for (int i = 0, dataSize = data.size(); i < dataSize; i++) {
                    data.set(i, evaluate(data.get(i), environment));
                }
                return ((LispFunction) lispObject1).execute(data, environment);
            }

            if (lispObject1 instanceof LispSpecialForm) {
                List<LispObject> data = ((LispList)((LispList) lispObject).cdr()).getData();
                return ((LispSpecialForm)lispObject1).execute(data, environment);
            }

            throw new RuntimeException("function or special form not found in environment");
        }
        if (lispObject instanceof LispSymbol) {
            return environment.find(((LispSymbol) lispObject).getName());
        }
        return null;

    }
}
