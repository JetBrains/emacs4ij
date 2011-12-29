package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 8:25 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Environment {
    protected boolean isRecording = false;
    protected ArrayList<String> myRecordedSymbols = new ArrayList<String>();
    protected HashMap<String, LispSymbol> mySymbols = new HashMap<String, LispSymbol>();
    protected boolean myArgumentsEvaluated = false;
    protected Environment myOuterEnv = null;
    protected String myBufferCurrentForEditing = null;

    public boolean areArgumentsEvaluated() {
        return myArgumentsEvaluated;
    }

    public void setArgumentsEvaluated(boolean argumentsEvaluated) {
        myArgumentsEvaluated = argumentsEvaluated;
    }

    public void setBufferCurrentForEditing (String buffer) {
        myBufferCurrentForEditing = buffer;
    }

    public String getBufferCurrentForEditing() {
        return myBufferCurrentForEditing == null ? GlobalEnvironment.INSTANCE.getCurrentBuffer() : myBufferCurrentForEditing;
    }

    public LispString getDefaultDirectory () {
        return (LispString) getBufferCurrentForEditing().getLocalVariableValue("directory");
    }

    public LObject find(String name, String methodName) {
        return find(name, methodName, null);
    }

    public LispSymbol find(String name) {
        return (LispSymbol) find(name, "", null);
    }

    public LObject find(String name, String methodName, Class[] parameterTypes, Object... methodParameters) {
        LispSymbol lispObject = mySymbols.get(name);

        if (lispObject != null) {
            if (!lispObject.isFunction() && lispObject.getValue() == null)
            {
                try {
                    lispObject = getBufferCurrentForEditing().getLocalVariable(name);
                } catch (NoOpenedBufferException e1) {
                    //return null;
                } catch (VoidVariableException e2) {
                    //return null;
                }
            }
            return lispObject.invokeMethod(methodName, parameterTypes, methodParameters);
        }
        if (myOuterEnv != null) {
            return myOuterEnv.find(name, methodName, parameterTypes, methodParameters);
        }
        return null;
    }

    public void startRecording() {
        isRecording = true;
        myRecordedSymbols.clear();
        myRecordedBuffers.clear();
    }

    public void stopRecording() {
        isRecording = false;
    }

    public void clearRecorded() {
        for (String name: myRecordedSymbols) {
            mySymbols.remove(name);
        }
        myRecordedSymbols.clear();
    }

    public void defineSymbol (LispSymbol symbol) {
        if (isRecording) {
            myRecordedSymbols.add(symbol.getName());
        }
        mySymbols.put(symbol.getName(), symbol);
    }

    protected boolean containsSymbol (String name) {
        return mySymbols.containsKey(name);
    }

    public void setVariable(LispSymbol symbol) {
        if (myOuterEnv == null || containsSymbol(symbol.getName())) {
            LispSymbol variable = mySymbols.get(symbol.getName());
            if (variable == null) {
                defineSymbol(symbol);
                return;
            }
            variable.setValue(symbol.getValue());
            defineSymbol(variable);
            return;
        }
        myOuterEnv.setVariable(symbol);
    }
}
