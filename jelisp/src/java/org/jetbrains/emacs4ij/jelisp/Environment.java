package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispBuiltinFunction;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSpecialForm;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class Environment {

    public static final Environment ourGlobal = new Environment(null);

    private final HashMap<String, LispObject> mySpecialForms = new HashMap<String, LispObject>();
    private HashMap<String, LispObject> myVariables = new HashMap<String, LispObject>();
    private HashMap<String, LispObject> myFunctions = new HashMap<String, LispObject>();
    private HashMap<String, LispObject> myBuiltinFunctions = new HashMap<String, LispObject>();

    private Environment myOuterEnv;

    public Environment (Environment outerEnv) {
        myOuterEnv = outerEnv;

        if (outerEnv == null) {
            setGlobal();
        }
    }

    private void setGlobal() {
        mySpecialForms.put("quote", new LispSpecialForm("quote"));
        mySpecialForms.put("defun", new LispSpecialForm("defun"));

        myBuiltinFunctions.put("+", new LispBuiltinFunction("+"));
        myBuiltinFunctions.put("*", new LispBuiltinFunction("*"));
        myBuiltinFunctions.put("set", new LispBuiltinFunction("set"));

    }

    public LispObject find(String name) {
        LispObject lispObject = mySpecialForms.get(name);
        if (lispObject != null)
            return lispObject;

        lispObject = myVariables.get(name);
        if (lispObject != null)
            return lispObject;

        lispObject = myBuiltinFunctions.get(name);
        if (lispObject != null)
            return lispObject;

        lispObject = myFunctions.get(name);
        if (lispObject != null)
            return lispObject;

        if (myOuterEnv != null) {
            return myOuterEnv.find(name);
        }

        throw new RuntimeException("unknown symbol " + name);
    }

    public void setVariable(LispObject name, LispObject value) {
        //TODO: check names to be unique
        myVariables.put(((LispSymbol)name).getName(), value);
    }

    public void defineFunction (LispObject name, LispObject value) {
        myFunctions.put(((LispSymbol)name).getName(), value);
    }

}
