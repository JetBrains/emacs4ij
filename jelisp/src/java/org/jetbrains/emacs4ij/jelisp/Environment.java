package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

import java.io.File;
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

    private HashMap<String, LispSymbol> mySymbols = new HashMap<String, LispSymbol>();
    /*private HashMap<String, LispSymbol> myVariables = new HashMap<String, LispSymbol>();
    private HashMap<String, LispSymbol> myBuiltinVariables = new HashMap<String, LispSymbol>();
    private HashMap<String, LispSymbol> myFunctions = new HashMap<String, LispSymbol>();
    private HashMap<String, LispSymbol> myBuiltinFunctions = new HashMap<String, LispSymbol>();*/
    //private HashMap<String, LispBuffer> myBuffers = new HashMap<String, LispBuffer>();
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
        mySymbols.put("quote", new LispSymbol("quote", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("defun", new LispSymbol("defun", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("defvar", new LispSymbol("defvar", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("defmacro", new LispSymbol("defmacro", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("let", new LispSymbol("let", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("let*", new LispSymbol("let*", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("interactive", new LispSymbol("interactive", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("or", new LispSymbol("or", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("and", new LispSymbol("and", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("if", new LispSymbol("if", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("while", new LispSymbol("while", LispSymbol.FunctionType.SpecialForm));
        mySymbols.put("cond", new LispSymbol("cond", LispSymbol.FunctionType.SpecialForm));

        mySymbols.put("+", new LispSymbol("+", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("*", new LispSymbol("*", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("set", new LispSymbol("set", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("eq", new LispSymbol("eq", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("car", new LispSymbol("car", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("cdr", new LispSymbol("cdr", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("car-safe", new LispSymbol("car-safe", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("cdr-safe", new LispSymbol("cdr-safe", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("memq", new LispSymbol("memq", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("list", new LispSymbol("list", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("null", new LispSymbol("null", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("not", new LispSymbol("not", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("stringp", new LispSymbol("stringp", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("subrp", new LispSymbol("subrp", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("symbolp", new LispSymbol("symbolp", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("integerp", new LispSymbol("integerp", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("symbol-function", new LispSymbol("symbol-function", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("get", new LispSymbol("get", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("put", new LispSymbol("put", LispSymbol.FunctionType.BuiltIn));

        //findAndRegisterEmacsFunction(ourFinder);

        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        mySymbols.put("void", LispSymbol.ourVoid);
        mySymbols.put("load-history", new LispSymbol("load-history", LispSymbol.ourNil));
        mySymbols.put("fill-column", new LispSymbol("fill-column", new LispInteger(70)));

        mySymbols.put("*scratch*",  new LispSymbol("*scratch*", new LispBuffer("*scratch*")));
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
    /*public LispList getFunctionFromFile(String fileName, String functionName) {
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
    } */

    public LispObject find(String name, String methodName) {
        return find(name, methodName, null);
    }

    public LispSymbol find(String name) {
        return (LispSymbol) find(name, "", null);
    }

    public LispObject find(String name, String methodName, Class[] parameterTypes, Object... methodParameters) {
        LispSymbol lispObject = mySymbols.get(name);
        if (lispObject != null)
            return lispObject.invokeMethod(methodName, parameterTypes, methodParameters);

        if (myOuterEnv != null) {
            return myOuterEnv.find(name, methodName, parameterTypes, methodParameters);
        }
        return null;
        //throw new RuntimeException("unknown symbol " + name);
    }

    /*private LispSymbol getSymbol(String name) {
        return mySymbols.get(name);
    } */

    public void defineSymbol (LispSymbol symbol) {
        mySymbols.put(symbol.getName(), symbol);
    }
}
