package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
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

    private HashMap<String, LispObject> myConstant;
    private HashMap<String, Object> mySpecialForm; //this list must be common for every program
    private HashMap<String, Object> myVariable;
    private HashMap<String, Object> myFunction;
    private StringBuilder myStackTrace;
    private ArrayList<LispObject> myCode; // the program

    public static final LispSymbol ourNilSymbol = new LispSymbol("nil");
    public static final LispSymbol ourTSymbol = new LispSymbol("t");


    public void appendConstant (String name, LispObject value) {

    }
}
