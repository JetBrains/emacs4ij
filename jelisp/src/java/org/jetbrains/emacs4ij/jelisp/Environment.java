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

    //private HashMap<String, LispObject> myBuilt;
    private final HashMap<String, LispObject> mySpecialForms = new HashMap<String, LispObject>();
    private HashMap<String, LispObject> myVariables = new HashMap<String, LispObject>();
    private HashMap<String, LispObject> myFunctions = new HashMap<String, LispObject>();

    private StringBuilder myStackTrace;
    private ArrayList<LispObject> myCode; // the program

    private Environment myOuterEnv;

    public Environment (Environment outerEnv) {
        myOuterEnv = outerEnv;

        if (outerEnv == null) {
            setGlobal();
        }
    }

    private void setGlobal() {
        mySpecialForms.put("quote", new LispSpecialForm("quote"));


    }


    public void appendConstant (String name, LispObject value) {

    }

    public LispObject find(String name) {
        LispObject lispObject = mySpecialForms.get(name);
        if (lispObject != null)
            return lispObject;

        lispObject = myVariables.get(name);
        if (lispObject != null)
            return lispObject;

        return new LispBuiltinFunction(name);
    }

    public void setVariable(LispObject name, LispObject value) {
        myVariables.put(((LispSymbol)name).getMyPrintName(), value);
    }
}
