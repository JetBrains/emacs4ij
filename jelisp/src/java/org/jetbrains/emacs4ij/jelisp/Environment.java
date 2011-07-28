package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

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
    private HashMap<LispSymbol, LispBuffer> myBuffers = new HashMap<LispSymbol, LispBuffer>();
    private Environment myOuterEnv;

    public static String ourEmacsPath = "";
    public static final LispSymbol ourFinder = new LispSymbol("find-lisp-object-file-name");
    private static final String ourFinderPath = "\\lisp\\help-fns.el";
    public static enum SymbolType {VARIABLE, FUNCTION, BUFFER}
    public static final Environment ourGlobal = new Environment(null);
    //public static JTextArea ourMiniBuffer;

    public Environment (Environment outerEnv) {
        myOuterEnv = outerEnv;
        if (myOuterEnv == null) {
            setGlobal();
        }
    }

    private void setGlobal() {

        mySpecialForms.put(new LispSymbol("quote"), new LispSpecialForm("quote"));
        mySpecialForms.put(new LispSymbol("defun"), new LispSpecialForm("defun"));
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

        myBuiltinVariables.put(LispSymbol.ourNil, LispSymbol.ourNil);
        myBuiltinVariables.put(LispSymbol.ourT, LispSymbol.ourT);
        myBuiltinVariables.put(new LispSymbol("load-history"), LispSymbol.ourNil);
        myBuiltinVariables.put(new LispSymbol("fill-column"), new LispInteger(70));

        myBuffers.put(new LispSymbol("*scratch*"), new LispBuffer("*scratch*"));
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

            case BUFFER:
                lispObject = myBuffers.get(lsName);
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
                lispObject = findAndRegisterEmacsFunction(lsName);
                if (lispObject != null)
                    return lispObject;
                throw new VoidFunctionException(name);
            case BUFFER:
                throw new RuntimeException("buffer " + name + "doesn't exist");

        }

        throw new RuntimeException("unknown symbol " + name);
    }

    private LispSymbol getKey (HashMap<LispSymbol, LispObject> where, LispSymbol key) {
        for (Map.Entry<LispSymbol, LispObject> entry : where.entrySet()) {
            if (entry.getKey().equals(key))
                return entry.getKey();
        }
        return LispSymbol.ourNil;
    }

    private boolean replaceKey(HashMap<LispSymbol, LispObject> where, LispSymbol newKey) {
        for (Map.Entry<LispSymbol, LispObject> entry : where.entrySet()) {
            if (entry.getKey().equals(newKey)) {
                LispObject value = entry.getValue();
                where.remove(newKey);
                where.put(newKey, value);
                return true;
            }
        }
        return false;
    }

    public LispSymbol findKeySymbol (LispSymbol key) {
        LispSymbol result = getKey(myBuiltinFunctions, key);
        if (result != LispSymbol.ourNil) return result;

        result = getKey(myBuiltinVariables, key);
        if (result != LispSymbol.ourNil) return result;

        result = getKey(myFunctions, key);
        if (result != LispSymbol.ourNil) return result;

        result = getKey(myVariables, key);
        if (result != LispSymbol.ourNil) return result;

        result = getKey(mySpecialForms, key);
        if (result != LispSymbol.ourNil) return result;

        if (myOuterEnv != null)
            return myOuterEnv.findKeySymbol(key);

        throw new RuntimeException("Variable or function with name " + key.getName() + " not registered");
    }

    public boolean replaceKeySymbol (LispSymbol key) {
        if (replaceKey(myBuiltinFunctions, key))
            return true;
        if (replaceKey(myBuiltinVariables, key))
            return true;
        if (replaceKey(myFunctions, key))
            return true;
        if (replaceKey(myVariables, key))
            return true;

        if (myOuterEnv != null)
            if (myOuterEnv.replaceKeySymbol(key))
                return true;

        throw new RuntimeException("Variable or function with name " + key.getName() + " not registered");
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

    public void defineFunction (LispObject name, LispCustomFunction value) {
        myFunctions.put((LispSymbol)name, value);
    }

    public static String read () {

        //TODO: new form
        throw new NotImplementedException();

        /*if (ourMiniBuffer == null)
            return null;

        ourMiniBuffer.setEditable(true);
        final boolean[] typed = {false};
        ourMiniBuffer.addKeyListener(new KeyListener() {
            public void keyTyped(KeyEvent e) {
            }

            public void keyPressed(KeyEvent e) {
            }

            public void keyReleased(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    typed[0] = true;
                }

            }
        });
        int oldLength = ourMiniBuffer.getText().length();
        while (true) {
            try {
                Thread.currentThread().sleep(2000);
            } catch (InterruptedException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
            System.out.println("f");
        }
        //ourMiniBuffer.setEditable(false);
        //return ourMiniBuffer.getText().substring(oldLength);
        //return  null;
        */
    }
}
