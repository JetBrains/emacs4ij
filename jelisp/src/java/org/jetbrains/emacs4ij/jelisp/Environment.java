package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;

import java.io.File;
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
    private HashMap<String, LispSymbol> mySymbols = new HashMap<String, LispSymbol>();
    private ArrayList<LispBuffer> myBuffers = new ArrayList<LispBuffer>();
    private Environment myOuterEnv;

    public static String ourEmacsPath = "";
    private final Environment myGlobalEnvironment;

    /**
     * Constructor for global environment
     */
    public Environment() {
        myGlobalEnvironment = this;
        setGlobal();
    }

    public Environment (@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;
        myGlobalEnvironment = outerEnv.getGlobalEnvironment();
    }

    public Environment getGlobalEnvironment() {
        return myGlobalEnvironment;
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

        mySymbols.put("bufferp", new LispSymbol("bufferp", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("current-buffer", new LispSymbol("current-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("buffer-size", new LispSymbol("buffer-size", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("buffer-name", new LispSymbol("buffer-name", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("get-buffer", new LispSymbol("get-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("set-buffer", new LispSymbol("set-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("switch-to-buffer", new LispSymbol("switch-to-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("other-buffer", new LispSymbol("other-buffer", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("point", new LispSymbol("point", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("point-min", new LispSymbol("point-min", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("point-max", new LispSymbol("point-max", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("buffer-end", new LispSymbol("buffer-end", LispSymbol.FunctionType.BuiltIn)); //note: it is compiled lisp function in emacs
        mySymbols.put("goto-char", new LispSymbol("goto-char", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("forward-char", new LispSymbol("forward-char", LispSymbol.FunctionType.BuiltIn));
        mySymbols.put("backward-char", new LispSymbol("backward-char", LispSymbol.FunctionType.BuiltIn));

        //findAndRegisterEmacsFunction(ourFinder);

        mySymbols.put("nil", LispSymbol.ourNil);
        mySymbols.put("t", LispSymbol.ourT);
        mySymbols.put("void", LispSymbol.ourVoid);
        mySymbols.put("load-history", new LispSymbol("load-history", LispSymbol.ourNil));
        mySymbols.put("fill-column", new LispSymbol("fill-column", new LispInteger(70)));



       // mySymbols.put("*scratch*",  new LispSymbol("*scratch*", new LispBuffer("*scratch*")));
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
    }

    public void defineSymbol (LispSymbol symbol) {
        mySymbols.put(symbol.getName(), symbol);
    }

    //============================= buffer processing =====================================

    public void defineBuffer (LispBuffer buffer) {
        try {
            if (getIndexByName(buffer.getName()) > -1) {
                throw new DoubleBufferException("double "+buffer.getName());
            }
        } catch (EnvironmentException e)  {
            myBuffers.add(buffer);
        }
    }

    public LispBuffer getCurrentBuffer () {
        if (myBuffers.size() == 0)
            throw new EnvironmentException("no buffer is currently opened");
        return myBuffers.get(myBuffers.size() - 1);
    }

    private int getIndexByName(String bufferName) {
        for (int i=0; i!= myBuffers.size(); ++i) {
            if (myBuffers.get(i).getName().equals(bufferName))
                return i;
        }
        throw new EnvironmentException("the buffer " + bufferName + " is not registered!");
    }

    public void setCurrentBuffer (String bufferName) {
        if (myBuffers.size() == 0)
            throw new EnvironmentException("no buffer is currently opened");
        if (myBuffers.get(myBuffers.size() - 1).getName().equals(bufferName))
            return;
        int newCurrentBufferIndex = getIndexByName(bufferName);
        LispBuffer newCurrentBuffer = myBuffers.get(newCurrentBufferIndex);
        myBuffers.remove(newCurrentBufferIndex);
        myBuffers.add(newCurrentBuffer);
    }

    private LispObject getBufferByName(String bufferName) {
        for (int i=0; i!= myBuffers.size(); ++i) {
            if (myBuffers.get(i).getName().equals(bufferName))
                myBuffers.get(i);
        }
        return LispSymbol.ourNil;
        //throw new EnvironmentException("the buffer " + bufferName + " is not registered!");
    }

    public LispObject getBuffer (String bufferName) {
        return getBufferByName(bufferName);
    }

    public LispBuffer getOtherBuffer () {
        if (myBuffers.size() < 2)
            //todo: what to return here?
            return getCurrentBuffer();
        return myBuffers.get(myBuffers.size() - 2);
    }

    public LispBuffer getOtherBuffer (String bufferName) {
        if (myBuffers.size() == 0)
            throw new RuntimeException("no buffer is currently opened");
        for (int i = myBuffers.size() - 1; i!=-1; --i) {
            if (!myBuffers.get(i).getName().equals(bufferName))
                return myBuffers.get(i);
        }
        //todo check what if there is only 1 buffer
        return null;
    }

    public int getBuffersSize() {
        return myBuffers.size();
    }

    public void closeBuffer(String bufferName) {
        int toRemove = getIndexByName(bufferName);
        myBuffers.remove(toRemove);
    }

    public void printBuffers() {
        for (int i=0; i!= myBuffers.size(); ++i) {
            System.out.print(myBuffers.get(i).getName()+"; ");
        }
        System.out.println();
    }

}
