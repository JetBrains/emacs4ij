package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 8:25 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Environment {
    protected boolean isRecording = false;
    protected ArrayList<String> myRecordedSymbols = new ArrayList<>();
    protected HashMap<String, LispSymbol> mySymbols = new HashMap<>();
    protected boolean myArgumentsEvaluated = false;
    protected Environment myOuterEnv = null;
    protected LispBuffer myBufferCurrentForEditing = null;
    protected boolean mySelectionManagedBySubroutine = false;
    protected static BufferManager ourBufferManager;
    protected static EmacsKeymapManager ourKeymapManager;

    public boolean isMainOrGlobal() {
        return (myOuterEnv == null || myOuterEnv.getOuterEnv() == null);
    }

    private Environment getOuterEnv() {
        return myOuterEnv;
    }
    
    public boolean areArgumentsEvaluated() {
        return myArgumentsEvaluated;
    }

    public void setArgumentsEvaluated(boolean argumentsEvaluated) {
        myArgumentsEvaluated = argumentsEvaluated;
    }

    public void setSelectionManagedBySubroutine(boolean selectionManagedBySubroutine) {
        mySelectionManagedBySubroutine = selectionManagedBySubroutine;
    }

    public boolean isSelectionManagedBySubroutine () {
        return mySelectionManagedBySubroutine;
    }

    public void setBufferCurrentForEditing (LispBuffer buffer) {
        myBufferCurrentForEditing = buffer;
    }

    public LispBuffer getBufferCurrentForEditing() {
        return myBufferCurrentForEditing == null ? ourBufferManager.getCurrentBuffer() : myBufferCurrentForEditing;
    }

    public LispString getDefaultDirectory () {
        return (LispString) getBufferCurrentForEditing().getLocalVariableValue("default-directory");
    }

    public LispSymbol find(String name) {
        LispSymbol symbol = mySymbols.get(name);
        if (symbol != null) {
            if (!symbol.isFunction() && symbol.isBufferLocal()) {
                try {
                    LispSymbol local = getBufferCurrentForEditing().getLocalVariable(name);
                    if (local.getValue() != null)
                        symbol = local;
                } catch (NoOpenedBufferException e1) {
                    //return null;
                } catch (VoidVariableException e2) {
                    //return null;
                }
            }
            return symbol;
        }
        if (myOuterEnv != null) {
            return myOuterEnv.find(name);
        }
        return null;
    }

    public void startRecording() {
        isRecording = true;
        myRecordedSymbols.clear();
        ourBufferManager.startRecording();
    }

    public void stopRecording() {
        isRecording = false;
    }

    public void clearRecorded() {
        for (String name: myRecordedSymbols) {
            mySymbols.remove(name);
        }
        myRecordedSymbols.clear();
        ourBufferManager.clearRecorded();
    }

    public void defineSymbol (LispSymbol symbol) {
        if (isRecording && !myRecordedSymbols.contains(symbol.getName())) {
            myRecordedSymbols.add(symbol.getName());
        }
        mySymbols.put(symbol.getName(), symbol);
    }

    public LispBuffer getOtherBuffer () {
        return ourBufferManager.getOtherBuffer(getBufferCurrentForEditing().getName());
    }

    public LispBuffer getOtherBuffer (LispBuffer buffer) {
        return ourBufferManager.getOtherBuffer(buffer.getName());
    }

    public LispBuffer createBuffer (String bufferName) {
        return ourBufferManager.createBuffer(bufferName);
    }

    public void switchToBuffer(String bufferName) {
        LispBuffer buffer = ourBufferManager.switchToBuffer(bufferName);
        if (buffer != null)
            setBufferCurrentForEditing(buffer);
    }

    public LispList getBufferList() {
        return ourBufferManager.getBufferList();
    }

    public void buryBuffer (LispBuffer buffer) {
        ourBufferManager.buryBuffer(buffer);
    }

    public void defineServiceBuffer (LispBuffer buffer) {
        GlobalEnvironment.INSTANCE.defineServiceBuffer(buffer);
    }

    public void defineBuffer (LispBuffer buffer) {
        GlobalEnvironment.INSTANCE.defineBuffer(buffer);
    }

    public void printBuffers() {
        ourBufferManager.printBuffers();
    }

    public void killBuffer (String bufferName) {
        ourBufferManager.killBuffer(findBufferSafe(bufferName));
    }

    public void killBuffer (LispBuffer buffer) {
        ourBufferManager.killBuffer(buffer);
    }

    public List<LispBuffer> getBuffers () {
        return ourBufferManager.getBuffers();
    }

    public int getBuffersSize() {
        return ourBufferManager.getBuffersSize();
    }

    public void closeCurrentBuffer () {
        ourBufferManager.removeBuffer(ourBufferManager.getCurrentBuffer());
    }

    public void closeAllBuffers () {
        ourBufferManager.closeAllBuffers();
    }

    public LispBuffer getBufferByIndex (int index) {
        return ourBufferManager.getBufferByIndex(index);
    }

    public String[] getBuffersNames () {
        return ourBufferManager.getBuffersNames();
    }

    public boolean isBufferDead (String bufferName) {
        return ourBufferManager.isDead(bufferName);
    }

    public boolean containsBuffer (String bufferName) {
        return ourBufferManager.containsBuffer(bufferName);
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
            return;
        }
        myOuterEnv.setVariable(symbol);
    }

    public LispBuffer getServiceBuffer (String bufferName) {
        return ourBufferManager.getServiceBuffer(bufferName);
    }

    public LispBuffer findBufferSafe(String bufferName) {
        return ourBufferManager.findBufferSafe(bufferName);
    }

    public LispBuffer findBuffer(String bufferName) {
        return ourBufferManager.findBuffer(bufferName);
    }

    public LispBuffer lastBuffer (String bufferName) {
        return ourBufferManager.lastBuffer(bufferName);
    }

    //========== mini buffer ==========================

    public LispMiniBuffer getMiniBuffer () {
        LispMiniBuffer miniBuffer = (LispMiniBuffer)ourBufferManager.getServiceBuffer(GlobalEnvironment.ourMiniBufferName);
        if (miniBuffer == null)
            throw new RuntimeException("mini buffer does not exist!");
        return miniBuffer;
    }

    public int getMiniBufferActivationsDepth() {
        try {
            return getMiniBuffer().getActivationsDepth();
        } catch (RuntimeException e) {
            return 0;
        }
    }
    
    //========= keymaps ===========
    public LispKeymap createKeymap (@Nullable LispObject name) {
        return ourKeymapManager.createKeymap(name);
    }

    public LispKeymap createSparseKeymap (@Nullable LispObject name) {
        return ourKeymapManager.createKeymap(name);
    }
    
    public LispKeymap getActiveKeymap() {
        return ourKeymapManager.getActiveKeymap();
    }
    
    public void setActiveKeymap(LispKeymap keymap) {
        ourKeymapManager.setActiveKeymap(keymap);
    }
}
