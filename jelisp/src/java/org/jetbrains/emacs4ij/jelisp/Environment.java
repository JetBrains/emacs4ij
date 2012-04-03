package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 8:25 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Environment {
    protected boolean isRecording = false;
    protected List<String> myRecordedSymbols = new ArrayList<>();
    protected Map<String, LispSymbol> mySymbols = new HashMap<>();
    protected boolean myArgumentsEvaluated = false;
    protected Environment myOuterEnv = null;
    protected LispBuffer myBufferCurrentForEditing = null;
    protected boolean mySelectionManagedBySubroutine = false;

    protected static EmacsKeymapManager ourKeymapManager;
    protected static FrameManager myFrameManager = null;

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

    @NotNull
    protected FrameManager getFrameManager() {
        if (myFrameManager == null)
            throw new InternalException(JelispBundle.message("frame.manager.not.initialized"));
        return myFrameManager;
    }

    protected BufferManager getBufferManager() {
        return getFrameManager().getCurrentBufferManager();
    }

    public LispBuffer getBufferCurrentForEditing() {
        return myBufferCurrentForEditing == null
                ? getBufferManager().getCurrentBuffer()
                : myBufferCurrentForEditing;
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
                } catch (NoOpenedBufferException | InternalException e) {
                    return symbol;
                } catch (VoidVariableException e1) {
                    return null;
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
    }

    public void clearRecorded() {
        for (String name: myRecordedSymbols) {
            mySymbols.remove(name);
        }
        myRecordedSymbols.clear();
    }

    public void defineSymbol (LispSymbol symbol) {
        if (isRecording && !myRecordedSymbols.contains(symbol.getName())) {
            myRecordedSymbols.add(symbol.getName());
        }
        mySymbols.put(symbol.getName(), symbol);
    }


    // =========== buffers =================
    public LispBuffer createBuffer (String bufferName) {
        return getBufferManager().createBuffer(bufferName);
    }

    public void onTabSwitch (String bufferName, Editor editor) {
        LispBuffer buffer = getBufferManager().switchToWindow(bufferName, editor);
        if (buffer != null)
            setBufferCurrentForEditing(buffer);
    }
    
    public void switchToBuffer(String bufferName) {
        LispBuffer buffer = getBufferManager().switchToBuffer(bufferName);
        if (buffer != null)
            setBufferCurrentForEditing(buffer);
    }

    public LispList getBufferList() {
        return LispList.list((List<LispObject>) getFrameManager().getBuffers());
    }

    public void buryBuffer (LispBuffer buffer) {
        getBufferManager().buryBuffer(buffer);
    }

    public void defineServiceBuffer (LispBuffer buffer) {
        getFrameManager().openServiceBuffer(buffer);
    }

    public void defineBuffer (LispBuffer buffer) {
        GlobalEnvironment.INSTANCE.defineBuffer(buffer);
    }

    public void killBuffer (String bufferName) {
        getFrameManager().killBuffer(findBufferSafe(bufferName));
    }

    public void killBuffer (LispBuffer buffer) {
        killBuffer(buffer.getName());
    }

    public List<LispBuffer> getBuffers () {
        return getFrameManager().getBuffers();
    }

    public int getBuffersSize() {
        return getBufferManager().getBuffersSize();
    }

    public void closeCurrentBuffer () {
        getBufferManager().closeCurrentBuffer();
    }

    public void closeAllBuffers () {
        getBufferManager().closeAllBuffers();
    }

    //test
    public LispBuffer getBufferByIndex (int index) {
        return getBufferManager().getBufferByIndex(index);
    }

    //test
    public String[] getBuffersNames () {
        return getBufferManager().getBuffersNames().toArray(new String[getBuffersSize()]);
    }

    public List<String> getBufferNamesList(String begin) {
        return getBufferManager().getBuffersNames(begin);
    }

    public boolean isBufferDead (String bufferName) {
        return getBufferManager().isDead(bufferName);
    }

    public boolean containsBuffer (String bufferName) {
        return getBufferManager().containsBuffer(bufferName);
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
        return getBufferManager().getServiceBuffer(bufferName);
    }

    public LispBuffer findBufferSafe(String bufferName) {
        return getBufferManager().findBufferSafe(bufferName);
    }

    public LispBuffer findBuffer(String bufferName) {
        return getBufferManager().findBuffer(bufferName);
    }

    public LispBuffer lastBuffer (String bufferName) {
        return getBufferManager().lastBuffer(bufferName);
    }

    public LispBuffer getOtherBuffer () {
        return getBufferManager().getOtherBuffer(getBufferCurrentForEditing().getName());
    }

    public LispBuffer getOtherBuffer (LispBuffer buffer) {
        return getBufferManager().getOtherBuffer(buffer.getName());
    }

    //========== mini buffer ==========================

    public LispMiniBuffer getMiniBuffer () {
        LispMiniBuffer miniBuffer = (LispMiniBuffer)getServiceBuffer(GlobalEnvironment.ourMiniBufferName);
        if (miniBuffer == null)
            throw new NoBufferException("minibuffer");
        return miniBuffer;
    }

    public int getMiniBufferActivationsDepth() {
        try {
            return getMiniBuffer().getActivationsDepth();
        } catch (NoBufferException e) {
            return 0;
        }
    }
    
    //========= keymaps ===========
    public LispKeymap createKeymap (@Nullable LispObject name) {
        return ourKeymapManager.createKeymap(name);
    }
    
    public LispKeymap getActiveKeymap() {
        return ourKeymapManager.getActiveKeymap();
    }
    
    public void setActiveKeymap(LispKeymap keymap) {
        ourKeymapManager.setActiveKeymap(keymap);
    }

    //========= frames ===================
    public void onFrameOpened (LispFrame newFrame) {
        getFrameManager().onFrameOpened(newFrame);
    }

    public void onFrameReleased (LispFrame frame) {
        getFrameManager().onFrameReleased(frame);
    }

    public void setSelectedFrame (LispFrame frame) {
        getFrameManager().setSelectedFrame(frame);
    }

    public LispFrame getSelectedFrame() {
        return getFrameManager().getSelectedFrame();
    }

    public void setFrameVisible (LispFrame frame, boolean status) {
        getFrameManager().setFrameVisible(frame, status);
    }

    public void setFrameIconified (LispFrame frame, boolean status) {
        getFrameManager().setFrameIconified(frame, status);
    }

    public boolean isFrameAlive (LispFrame frame) {
        return getFrameManager().isFrameAlive(frame);
    }

    public List<LispFrame> getVisibleFrames () {
        return getFrameManager().getVisibleFrames();
    }

    public List<LispFrame> getVisibleAndIconifiedFrames () {
        return getFrameManager().getVisibleAndIconifiedFrames();
    }

    public List<LispFrame> getAllFrames () {
        return getFrameManager().getAllFrames();
    }
}
