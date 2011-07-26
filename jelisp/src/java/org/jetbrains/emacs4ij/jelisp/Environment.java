package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.io.*;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class Environment {
    private final HashMap<LispSymbol, LispObject> mySpecialForms = new HashMap<LispSymbol, LispObject>();
    private HashMap<LispSymbol, LispObject> myVariables = new HashMap<LispSymbol, LispObject>();
    private HashMap<LispSymbol, LispObject> myBuiltinVariables = new HashMap<LispSymbol, LispObject>();
    private HashMap<LispSymbol, LispObject> myFunctions = new HashMap<LispSymbol, LispObject>();
    private HashMap<LispSymbol, LispObject> myBuiltinFunctions = new HashMap<LispSymbol, LispObject>();
    private HashMap<LispSymbol, LispCustomFunction> myEmacsFunctions = new HashMap<LispSymbol, LispCustomFunction>();

    private static final String ourEmacsPath = findEmacs();
    public static final LispSymbol ourFinder = new LispSymbol("find-lisp-object-file-name");
    private static final String ourFinderPath = "lisp\\help-fns.el";

    private Environment myOuterEnv;

    public static enum SymbolType {VARIABLE, FUNCTION}
    public static final Environment ourGlobal = new Environment(null);

    public Environment (Environment outerEnv) {
        myOuterEnv = outerEnv;

        if (outerEnv == null) {
            setGlobal();
        }
    }

    private void setGlobal() {
        mySpecialForms.put(new LispSymbol("quote"), new LispSpecialForm("quote"));
        mySpecialForms.put(new LispSymbol("defun"), new LispSpecialForm("defun"));
        mySpecialForms.put(new LispSymbol("let"), new LispSpecialForm("let"));
        mySpecialForms.put(new LispSymbol("let*"), new LispSpecialForm("let*"));

        mySpecialForms.put(new LispSymbol("interactive"), new LispSpecialForm("interactive"));

        myBuiltinFunctions.put(new LispSymbol("+"), new LispBuiltinFunction("+"));
        myBuiltinFunctions.put(new LispSymbol("*"), new LispBuiltinFunction("*"));
        myBuiltinFunctions.put(new LispSymbol("set"), new LispBuiltinFunction("set"));

        myEmacsFunctions.put(ourFinder, findEmacsFunction(ourFinder));

        myBuiltinVariables.put(LispSymbol.ourNil, LispSymbol.ourNil);
        myBuiltinVariables.put(LispSymbol.ourT, LispSymbol.ourT);
    }

    private static String findEmacs() {
        return  "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3\\";
    }

    //TODO: its public only for test
    public LispList getFunctionFromFile(String fileName, String functionName) {
        File file = new File(fileName);
        BufferedReader reader;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            throw new RuntimeException("File not found: " + fileName);
        }
        String line;
        while (true) {
            try {
                line = reader.readLine();
            } catch (IOException e) {
                throw new RuntimeException("Error while reading " + fileName);
            }
            if (line == null)
                throw new RuntimeException("function " + functionName + " not found in " + fileName);
            if (line.contains("(defun " + functionName))
                break;
        }

        BufferedReaderParser p = new BufferedReaderParser(reader);

        LispObject parsed = p.parse(line);

        if (parsed instanceof LispList) {

            if (((LispSymbol)((LispList) parsed).car()).getName().equals("defun"))
                return (LispList) parsed;
            throw new RuntimeException("Parsed list is not a function definition!");
        }
        throw new RuntimeException("Parsed object is not a LispList!");
    }

    //TODO: its public only for test
    public String findEmacsFunctionFileName(String functionName) {
        LispCustomFunction finder = myEmacsFunctions.get(ourFinder);
        if (finder == null)
            return ourEmacsPath + ourFinderPath;

        return null;
    }

    public LispCustomFunction findEmacsFunction (LispSymbol name) {
        LispCustomFunction emacsFunction = myEmacsFunctions.get(name);
        if (emacsFunction != null)
            return emacsFunction;
        String path = findEmacsFunctionFileName(name.getName());
        LispList function = getFunctionFromFile(path, name.getName());
        if (function.cdr() instanceof LispList) {
            LispCustomFunction f = new LispCustomFunction(((LispList) function.cdr()).getData());
            myEmacsFunctions.put(name, f);
            return f;
        }
        throw new RuntimeException("Wrong defun construction: no arguments");
    }

    public LispObject find(String name, SymbolType symbolType) {
        LispSymbol lsName = new LispSymbol(name);
        LispObject lispObject;

        switch (symbolType) {
            case VARIABLE:
                lispObject = myBuiltinVariables.get(lsName);
                if (lispObject != null)
                    return lispObject;

                lispObject = myVariables.get(lsName);
                if (lispObject != null)
                    return lispObject;

            case FUNCTION:
                lispObject = mySpecialForms.get(lsName);
                if (lispObject != null)
                    return lispObject;

                lispObject = myBuiltinFunctions.get(lsName);
                if (lispObject != null)
                    return lispObject;

                lispObject = myFunctions.get(lsName);
                if (lispObject != null)
                    return lispObject;
        }

        if (myOuterEnv != null) {
            return myOuterEnv.find(name, symbolType);
        }

        switch (symbolType) {
            case VARIABLE:
                throw new VoidVariableException(name);
            case FUNCTION:
                lispObject = findEmacsFunction(lsName);
                if (lispObject != null)
                    return lispObject;
                throw new VoidFunctionException(name);
        }

        throw new RuntimeException("unknown symbol " + name);
    }


    public void setVariable(LispObject name, LispObject value) {
        myVariables.put((LispSymbol)name, value);
    }

    public LispObject getVariable (String name) {
        return getVariable(new LispSymbol(name));
    }

    public LispObject getVariable(LispSymbol name) {
        return myVariables.get(name);
    }

    public void defineFunction (LispObject name, LispObject value) {
        myFunctions.put((LispSymbol)name, value);
    }

}
