package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.io.*;
import java.util.HashMap;

//import com.intellij.openapi.editor.Editor;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class Environment {
    //public static com.intellij.openapi.editor.Editor ourEditor;

    private final HashMap<LispSymbol, NamedLispObject> mySpecialForms = new HashMap<LispSymbol, NamedLispObject>();
    private HashMap<LispSymbol, NamedLispObject> myVariables = new HashMap<LispSymbol, NamedLispObject>();
    private HashMap<LispSymbol, NamedLispObject> myBuiltinVariables = new HashMap<LispSymbol, NamedLispObject>();
    private HashMap<LispSymbol, NamedLispObject> myFunctions = new HashMap<LispSymbol, NamedLispObject>();
    private HashMap<LispSymbol, NamedLispObject> myBuiltinFunctions = new HashMap<LispSymbol, NamedLispObject>();
    private HashMap<LispSymbol, LispBuffer> myBuffers = new HashMap<LispSymbol, LispBuffer>();
    private Environment myOuterEnv;

    public static String ourEmacsPath = "";
    public static final LispSymbol ourFinder = new LispSymbol("find-lisp-object-file-name");
    private static final String ourFinderPath = "\\lisp\\help-fns.el";
    public static enum SymbolType {VARIABLE, FUNCTION, BUFFER, ANY}
    public static final Environment ourGlobal = new Environment(null);

    public Environment (Environment outerEnv) {
        myOuterEnv = outerEnv;
        if (myOuterEnv == null) {
            setGlobal();
        }
    }

    private void setGlobal() {

        mySpecialForms.put(new LispSymbol("quote"), new LispSpecialForm("quote"));
        mySpecialForms.put(new LispSymbol("defun"), new LispSpecialForm("defun"));
        mySpecialForms.put(new LispSymbol("defvar"), new LispSpecialForm("defvar"));
        mySpecialForms.put(new LispSymbol("let"), new LispSpecialForm("let"));
        mySpecialForms.put(new LispSymbol("let*"), new LispSpecialForm("let*"));
        mySpecialForms.put(new LispSymbol("interactive"), new LispSpecialForm("interactive"));
        mySpecialForms.put(new LispSymbol("or"), new LispSpecialForm("or"));
        mySpecialForms.put(new LispSymbol("and"), new LispSpecialForm("and"));
        mySpecialForms.put(new LispSymbol("if"), new LispSpecialForm("if"));
        mySpecialForms.put(new LispSymbol("while"), new LispSpecialForm("while"));
        mySpecialForms.put(new LispSymbol("cond"), new LispSpecialForm("cond"));

        myBuiltinFunctions.put(new LispSymbol("+"), new LispBuiltinFunction("+"));
        myBuiltinFunctions.put(new LispSymbol("*"), new LispBuiltinFunction("*"));
        myBuiltinFunctions.put(new LispSymbol("set"), new LispBuiltinFunction("set"));
        myBuiltinFunctions.put(new LispSymbol("eq"), new LispBuiltinFunction("eq"));
        myBuiltinFunctions.put(new LispSymbol("car-safe"), new LispBuiltinFunction("car-safe"));
        myBuiltinFunctions.put(new LispSymbol("cdr-safe"), new LispBuiltinFunction("cdr-safe"));
        myBuiltinFunctions.put(new LispSymbol("memq"), new LispBuiltinFunction("memq"));
        myBuiltinFunctions.put(new LispSymbol("list"), new LispBuiltinFunction("list"));
        myBuiltinFunctions.put(new LispSymbol("null"), new LispBuiltinFunction("null"));
        myBuiltinFunctions.put(new LispSymbol("not"), new LispBuiltinFunction("not"));
        myBuiltinFunctions.put(new LispSymbol("car"), new LispBuiltinFunction("car"));
        myBuiltinFunctions.put(new LispSymbol("cdr"), new LispBuiltinFunction("cdr"));
        myBuiltinFunctions.put(new LispSymbol("stringp"), new LispBuiltinFunction("stringp"));
        myBuiltinFunctions.put(new LispSymbol("symbol-function"), new LispBuiltinFunction("symbol-function"));
        myBuiltinFunctions.put(new LispSymbol("subrp"), new LispBuiltinFunction("subrp"));
        myBuiltinFunctions.put(new LispSymbol("symbolp"), new LispBuiltinFunction("symbolp"));
        myBuiltinFunctions.put(new LispSymbol("integerp"), new LispBuiltinFunction("integerp"));
        myBuiltinFunctions.put(new LispSymbol("get"), new LispBuiltinFunction("get"));
        myBuiltinFunctions.put(new LispSymbol("put"), new LispBuiltinFunction("put"));

        //findAndRegisterEmacsFunction(ourFinder);

        myBuiltinVariables.put(LispSymbol.ourNil,  new LispVariable(LispSymbol.ourNil, LispSymbol.ourNil));
        myBuiltinVariables.put(LispSymbol.ourT, new LispVariable(LispSymbol.ourT, LispSymbol.ourT));
        myBuiltinVariables.put(new LispSymbol("load-history"), new LispVariable(new LispSymbol("load-history"), LispSymbol.ourNil));
        myBuiltinVariables.put(new LispSymbol("fill-column"), new LispVariable(new LispSymbol("fill-column"), new LispInteger(70)));

        myBuffers.put(new LispSymbol("*scratch*"), new LispBuffer("*scratch*"));
    }

    private void indexEmacsSources() {
        if (ourEmacsPath.equals("")) {
            ourEmacsPath = "c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3";
            //TODO:
            //throw new RuntimeException("Emacs path is not set!");
        }
        indexDirectory(ourEmacsPath + "\\lisp");
    }

    private void indexDirectory (String dir) {
        File[] fList = new File(dir).listFiles();
        for (File f: fList) {
            if (f.isDirectory())
                indexDirectory(f.getAbsolutePath());
            else if (f.getName().substring(f.getName().lastIndexOf('.'), f.getName().length()).equals("el")) {

                //TODO: parse and save
            }
        }


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

    //TODO: it is public only for test
    public String findEmacsFunctionFileName(String functionName) {
        if (ourEmacsPath.equals("")) {
            throw new RuntimeException("Emacs path is not set!");
        }

        LispCustomFunction finder = (LispCustomFunction) myFunctions.get(ourFinder);

        if (finder == null) {
            if (functionName.equals(ourFinder.getName()))
                return ourEmacsPath + ourFinderPath;
        }
        if (functionName.equals("symbol-file"))
            return ourEmacsPath + "\\lisp\\subr.el";

        //TODO: eval finder

        throw new RuntimeException("I don't know where to find function " + functionName);

    }

    public LispCustomFunction findAndRegisterEmacsFunction (LispSymbol name) {
        LispCustomFunction emacsFunction = (LispCustomFunction) myFunctions.get(name);
        if (emacsFunction != null)
            return emacsFunction;
        String path = findEmacsFunctionFileName(name.getName());
        LispList function = getFunctionFromFile(path, name.getName());
        LispObject evaluated = function.evaluate(this);
        if (!name.equals(evaluated)) {
            throw new RuntimeException("findAndRegisterEmacsFunction FAILED : " + name.getName());
        }
        return (LispCustomFunction) myFunctions.get(name);
    }

    public LispObject find(String name, SymbolType symbolType, String methodName) {
        return find(name, symbolType, methodName, null);
    }

    public LispObject find(String name, SymbolType symbolType) {
        return find(name, symbolType, "", null);
    }

    public LispObject find(String name, SymbolType symbolType, String methodName, Class[] parameterTypes, Object... methodParameters) {
        LispSymbol lsName = new LispSymbol(name);
        NamedLispObject lispObject;

        switch (symbolType) {
            case ANY:
            case VARIABLE:
                lispObject = myBuiltinVariables.get(lsName);
                if (lispObject != null)
                    return lispObject.invokeMethod(methodName, parameterTypes,methodParameters);

                lispObject = myVariables.get(lsName);
                if (lispObject != null)
                    return lispObject.invokeMethod(methodName, parameterTypes,methodParameters);

            case FUNCTION:
                lispObject = mySpecialForms.get(lsName);
                if (lispObject != null)
                    return lispObject.invokeMethod(methodName, parameterTypes,methodParameters);

                lispObject = myBuiltinFunctions.get(lsName);
                if (lispObject != null)
                    return lispObject.invokeMethod(methodName, parameterTypes,methodParameters);

                lispObject = myFunctions.get(lsName);
                if (lispObject != null)
                    return lispObject.invokeMethod(methodName, parameterTypes,methodParameters);

            case BUFFER:
                lispObject = myBuffers.get(lsName);
                if (lispObject != null)
                    return lispObject.invokeMethod(methodName, parameterTypes,methodParameters);
        }

        if (myOuterEnv != null) {
            return myOuterEnv.find(name, symbolType, methodName, parameterTypes, methodParameters);
        }

        switch (symbolType) {
            case VARIABLE:
                throw new VoidVariableException(name);
            case FUNCTION:
                lispObject = findAndRegisterEmacsFunction(lsName);
                if (lispObject != null)
                    return lispObject;
                throw new VoidFunctionException(name);
            case BUFFER:
                throw new RuntimeException("buffer " + name + "doesn't exist");

        }

        throw new RuntimeException("unknown symbol " + name);
    }

    public void defineVariable (LispObject name, LispObject value) {
        myVariables.put((LispSymbol) name, new LispVariable((LispSymbol) name, value));
    }

    public void defineVariable(LispObject name, NamedLispObject value) {
        myVariables.put((LispSymbol) name, value);
    }

    public NamedLispObject getVariable (String name) {
        return getVariable(new LispSymbol(name));
    }

    public NamedLispObject getVariable(LispSymbol name) {
        return myVariables.get(name);
    }

    public void defineFunction (LispSymbol name, LispCustomFunction value) {
        myFunctions.put(name, value);
    }
}
