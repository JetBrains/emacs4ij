package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.*;

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
    protected HashMap<String, LispSymbol> mySymbols = new HashMap<String, LispSymbol>();
    protected Environment myOuterEnv;
    private boolean mySelectionManagedBySubroutine = false;
    private boolean myArgumentsEvaluated = false;
    protected LispBuffer myBufferCurrentForEditing = null;

    // for global environment
    protected Environment () {}

    public Environment (@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;
    }

    public boolean isGlobalEnvironment() {
        return myOuterEnv == null;
    }

    public void setSelectionManagedBySubroutine(boolean selectionManagedBySubroutine) {
       mySelectionManagedBySubroutine = selectionManagedBySubroutine;
    }

    public boolean isSelectionManagedBySubroutine () {
        return mySelectionManagedBySubroutine;
    }

    public boolean areArgumentsEvaluated() {
        return myArgumentsEvaluated;
    }

    public void setArgumentsEvaluated(boolean argumentsEvaluated) {
        myArgumentsEvaluated = argumentsEvaluated;
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
            if (lispObject.getValue() != null && lispObject.getValue().equals(LispSymbol.ourBufferLocalVariable)) {
                lispObject = getBufferCurrentForEditing().getLocalVariable(name);
            }
            return lispObject.invokeMethod(methodName, parameterTypes, methodParameters);
        }
        if (myOuterEnv != null) {
            return myOuterEnv.find(name, methodName, parameterTypes, methodParameters);
        }
        return null;
    }

    public void defineSymbol (LispSymbol symbol) {
        mySymbols.put(symbol.getName(), symbol);
    }

    protected boolean containsSymbol (String name) {
        return mySymbols.containsKey(name);
    }

    public void setVariable(LispSymbol symbol) {
        if (isGlobalEnvironment() || containsSymbol(symbol.getName())) {
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

    public boolean isMainEnvironment() {
        return myOuterEnv instanceof GlobalEnvironment;
    }

    public void updateFunction (LispSymbol symbol) {
        GlobalEnvironment.getInstance().updateFunction(symbol);
    }

    public void defineBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().defineBuffer(buffer);
    }

    public LispBuffer createBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().createBuffer(bufferName);
    }

    public void defineServiceBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().defineServiceBuffer(buffer);
    }

    public void updateBuffer(LispBuffer buffer) {
        GlobalEnvironment.getInstance().updateBuffer(buffer);
    }

    public void updateServiceBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().updateServiceBuffer(buffer);
    }

    public void switchToBuffer(String bufferName) {
        GlobalEnvironment.getInstance().switchToBuffer(bufferName);
    }

    public void setBufferCurrentForEditing (LispBuffer buffer) {
        myBufferCurrentForEditing = buffer;
    }

    public LispBuffer getBufferCurrentForEditing() {
        if (myBufferCurrentForEditing == null) {
            return GlobalEnvironment.getInstance().getCurrentBuffer();
        }
        return myBufferCurrentForEditing;
    }

    public LispBuffer findBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().findBuffer(bufferName);
    }

    public LispBuffer getServiceBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().getServiceBuffer(bufferName);
    }


    public ArrayList<LispBuffer> getBuffers () {
        return GlobalEnvironment.getInstance().getBuffers();
    }

    public LispBuffer getOtherBuffer () {
        return getOtherBuffer(getBufferCurrentForEditing().getName());
    }

    public LispBuffer getOtherBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().getOtherBuffer(bufferName);
    }

    public int getBuffersSize() {
        return GlobalEnvironment.getInstance().getBuffersSize();
    }

    public LispList getBufferList() {
        return GlobalEnvironment.getInstance().getBufferList();
    }

    public void closeCurrentBuffer () {
        GlobalEnvironment.getInstance().closeCurrentBuffer();
    }

    public void killBuffer (String bufferName) {
        GlobalEnvironment.getInstance().killBuffer(bufferName);
    }

    public void killBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().killBuffer(buffer);
    }

    public void closeAllBuffers () {
        GlobalEnvironment.getInstance().closeAllBuffers();
    }

    public LispBuffer getBufferByIndex (int index) {
        return GlobalEnvironment.getInstance().getBufferByIndex(index);
    }

    public void printBuffers() {
        GlobalEnvironment.getInstance().printBuffers();
    }

    public String[] getBuffersNames () {
        return GlobalEnvironment.getInstance().getBuffersNames();
    }

    public LispString getDefaultDirectory () {
        return (LispString) getBufferCurrentForEditing().getLocalVariableValue("directory");
    }

    public void buryBuffer (LispBuffer buffer) {
        GlobalEnvironment.getInstance().buryBuffer(buffer);
    }

    public LispBuffer lastBuffer () {
        return lastBuffer("");
    }

    public LispBuffer lastBuffer (String bufferName) {
        return GlobalEnvironment.getInstance().lastBuffer(bufferName);
    }

    public boolean containsBuffer (String bufferName) {
        return findBuffer(bufferName) != null;
    }

    public boolean isDead (String bufferName) {
        return GlobalEnvironment.getInstance().isDead(bufferName);
    }

    public ArrayList<String> getCommandList (String begin) {
        return GlobalEnvironment.getInstance().getCommandList(begin);
    }

    //========== mini buffer ==========================

    public LispMiniBuffer getMiniBuffer () {
        LispMiniBuffer miniBuffer = (LispMiniBuffer) getServiceBuffer(GlobalEnvironment.ourMiniBufferName);
        if (miniBuffer == null)
            throw new RuntimeException("mini buffer does not exist!");
        return miniBuffer;
    }
}
