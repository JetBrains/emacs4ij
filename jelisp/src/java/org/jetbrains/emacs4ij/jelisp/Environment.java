package org.jetbrains.emacs4ij.jelisp;

import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSyntaxTable;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.*;

import java.util.*;

public abstract class Environment {
    protected boolean isRecording = false;
    protected List<String> myRecordedSymbols = new ArrayList<>();
    protected Map<String, LispSymbol> mySymbols = new HashMap<>();
    private boolean myArgumentsEvaluated = false;
    private boolean mySpecFormsAndMacroAllowed = true;
    protected Environment myOuterEnv = null;
    protected LispBuffer myBufferCurrentForEditing = null;
    protected boolean mySelectionManagedBySubroutine = false;

    protected static EmacsKeymapManager ourKeymapManager;
    protected static FrameManager ourFrameManager = null;
    protected static BufferManager ourBufferManager = null;
    protected static WindowManager ourWindowManager = null;

    public boolean isMainOrGlobal() {
        return myOuterEnv == null || myOuterEnv.getOuterEnv() == null;
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

    public boolean areSpecFormsAndMacroAllowed() {
        return mySpecFormsAndMacroAllowed;
    }

    public void setSpecFormsAndMacroAllowed(boolean mySpecFormsAndMacroAllowed) {
        this.mySpecFormsAndMacroAllowed = mySpecFormsAndMacroAllowed;
    }

    public void setBufferCurrentForEditing (LispBuffer buffer) {
        myBufferCurrentForEditing = buffer;
    }

    public LispBuffer getBufferCurrentForEditing() {
        return myBufferCurrentForEditing == null
                ? ourBufferManager.getCurrent()
                : myBufferCurrentForEditing;
    }

    public LispSymbol find(String name) {
        LispSymbol symbol = mySymbols.get(name);
        if (symbol != null) {
            return symbol;
        }
        if (myOuterEnv != null) {
            return myOuterEnv.find(name);
        }
        return null;
    }

    public void startRecording() {
        isRecording = true;
        clearRecorded();
    }

    public void clearRecorded() {
        for (String name: myRecordedSymbols) {
            mySymbols.remove(name);
        }
        myRecordedSymbols.clear();
        ourBufferManager.clear();
        ourWindowManager.clear();
    }

    public void remove (String name) {
        mySymbols.remove(name);
        myRecordedSymbols.remove(name);
    }

    public void clear() {
        //todo: don't erase "permanent" vars and hooks with permanent-local-hook property != nil
        mySymbols.clear();
        myRecordedSymbols.clear();
    }

    protected void putSymbol (LispSymbol symbol) {
        if (symbol.getName().startsWith(":"))
            symbol.setConstant();
        mySymbols.put(symbol.getName(), symbol);
    }

    public void defineSymbol (LispSymbol symbol) {
        if (isRecording && !myRecordedSymbols.contains(symbol.getName())) {
            myRecordedSymbols.add(symbol.getName());
        }
        putSymbol(symbol);
    }

    // =========== buffers =================
    public LispBuffer createBuffer (String bufferName) {
        return ourBufferManager.createBuffer(bufferName);
    }

    public LispBuffer createToolBuffer (String bufferName, @NotNull LispToolWindow window) {
        return ourBufferManager.createBuffer(bufferName, window);
    }

    public LispWindow getBufferLastSelectedWindow (LispBuffer buffer) {
        return ourWindowManager.getBufferLastSelectedWindow(buffer);
    }

    public void onTabSwitch (EditorWrapper editor) {
        switchToWindow(editor, true);
    }

    /**
     * this method switches only for "buffer ring", but doesn't perform visual switch and doesn't set current
     * corresponding window & frame
     */
    public void switchToBuffer(LispBuffer buffer) {
        ourBufferManager.switchTo(buffer);
    }

    /**
     * this method switches only for "window ring", but doesn't perform visual switch
     * also the displayed buffer is made current and frame which holds window with given editor
     */
    public void switchToWindow(EditorWrapper editor, boolean switchBuffer) {
        LispWindow window = ourWindowManager.getEditorWindow(editor);
        ourWindowManager.switchTo(window);
        switchToFrame(window.getFrame());
        setBufferCurrentForEditing(window.getBuffer());
        if (switchBuffer)
            ourBufferManager.switchTo(window.getBuffer());
    }

    public void switchToWindow (LispWindow window) {
        ourWindowManager.switchTo(window);
        setBufferCurrentForEditing(ourBufferManager.switchTo(window.getBuffer()));
    }

    /**
     * this method switches only for "frame ring", but doesn't perform visual switch
     */
    public void switchToFrame (LispFrame frame) {
        ourFrameManager.switchTo(frame);
    }

    public LispList getBufferList() {
        List<LispBuffer> data = new ArrayList<>(ourBufferManager.getData());
        return LispList.list(data.toArray(new LispObject[data.size()]));
    }
    
    /**
     * 
     * @param frame 
     * @return list of buffers where buffers displayed on frame come first (in order of opening) and then the rest 
     */
    public LispList getBufferList (LispFrame frame) {
        List<LispBuffer> all = ourBufferManager.getData();
        List<LispBuffer> frameBuffers = Arrays.asList(ourWindowManager.getFrameBuffers(frame));
        List<LispObject> list = new ArrayList<>();
        for (LispBuffer buffer: all) {
            if (frameBuffers.contains(buffer))
                list.add(buffer);
        }
        for (LispBuffer buffer: all) {
            if (!list.contains(buffer))
                list.add(buffer);
        }
        return LispList.list(list);
    }

    public boolean containsBuffer (String bufferName) {
        return ourBufferManager.containsBuffer(bufferName);
    }

    public void buryBuffer (LispBuffer buffer) {
        ourBufferManager.bury(buffer);
    }

    public void defineBuffer (LispBuffer buffer) {
        GlobalEnvironment.INSTANCE.defineBuffer(buffer);
    }

    public void hideBuffer (String bufferName) {
        ourWindowManager.hideBufferOnFrame(findBufferSafe(bufferName), getSelectedFrame());
    }

    public void killBuffer (LispBuffer buffer) {
        if (buffer instanceof LispMinibuffer)
            buffer.kill();
        else ourBufferManager.killBuffer(buffer);
        ourWindowManager.onKillBuffer(buffer);
    }

    public void hideBuffer (LispBuffer buffer) {
        ourWindowManager.hideBufferOnFrame(buffer, getSelectedFrame());
    }

    public List<LispBuffer> getBuffers () {
        return ourBufferManager.getData();
    }

    public int getBuffersSize() {
        return ourBufferManager.getSize();
    }

    //for test
    public void closeCurrentBuffer () {
        killBuffer(ourBufferManager.getCurrent());
        myBufferCurrentForEditing = ourBufferManager.isEmpty() ? null : ourBufferManager.getCurrent();
    }

    //test
    public void closeAllBuffers () {
        ourBufferManager.clear();
        myBufferCurrentForEditing = null;
    }

    //test
    public LispBuffer getBufferByIndex (int index) {
        return ourBufferManager.getBufferByIndex(index);
    }

    //test
    public String[] getBuffersNames () {
        return ourBufferManager.getBuffersNames().toArray(new String[getBuffersSize()]);
    }

    public List<String> getBufferNamesList(String begin) {
        return ourBufferManager.getBuffersNames(begin);
    }

    public boolean isBufferAlive(LispBuffer buffer) {
        return ourBufferManager.isAlive(buffer);
    }

    public boolean containsSymbol (String name) {
        return mySymbols.containsKey(name);
    }

    public void setVariable(LispSymbol symbol) {
        String name = symbol.getName();
        if (myOuterEnv == null || containsSymbol(name) ||
                (this instanceof BufferEnvironment && GlobalEnvironment.INSTANCE.isVariableBufferLocal(name))) {
            LispSymbol variable = mySymbols.get(name);
            if (variable == null) {
                defineSymbol(symbol);
                return;
            }
            variable.setValue(symbol.getValue());
            return;
        }
        myOuterEnv.setVariable(symbol);
    }

    @NotNull
    public LispBuffer findBufferSafe(String bufferName) {
        return ourBufferManager.findBufferSafe(bufferName);
    }

    @Nullable
    public LispBuffer findBuffer(String bufferName) {
        return ourBufferManager.findBuffer(bufferName);
    }

    @Nullable
    public LispBuffer getOtherBuffer (LispBuffer buffer, LispFrame frame, boolean invisibleBuffersPreferred) {
        return ourWindowManager.getFrameOtherBuffer(buffer, frame, invisibleBuffersPreferred);
    }

    public LispBuffer getEditorBuffer (EditorWrapper editor) {
        return ourWindowManager.getEditorWindow(editor).getBuffer();
    }

    //========== mini buffer ==========================

    @NotNull
    public LispMinibuffer getMinibuffer() {
        LispMinibuffer miniBuffer = ourBufferManager.getMinibuffer();
        if (miniBuffer == null)
            throw new UnregisteredBufferException("minibuffer");
        return miniBuffer;
    }

    public int getMiniBufferActivationsDepth() {
        try {
            return getMinibuffer().getActivationsDepth();
        } catch (UnregisteredBufferException e) {
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

    public void setActiveKeymap(String keymapName) {
        ourKeymapManager.setActiveKeymap(keymapName);
    }

    public boolean isKeymapActive (LispKeymap keymap) {
        return ourKeymapManager.isKeymapActive(keymap);
    }

    //========= frames & windows ===================
    public void onBufferOpened(LispBuffer buffer, EditorWrapper editor) {
        ourWindowManager.onOpenBuffer(buffer, getSelectedFrame(), editor);
    }

    public void onToolBufferOpened(LispToolWindow window) {
        ourWindowManager.onOpenToolBuffer(getSelectedFrame(), window);
    }

    public void onFrameOpened (LispFrame newFrame) {
        ourFrameManager.onFrameOpened(newFrame);
    }

    public void onFrameReleased (LispFrame frame) {
        ourFrameManager.onFrameReleased(frame);
    }

    public void setSelectedFrame (LispFrame frame) {
        ourFrameManager.switchTo(frame);
    }

    @NotNull
    public LispFrame getExistingFrame (LispFrame frame) {
        return ourFrameManager.getExistingFrame(frame);
    }

    @NotNull
    public LispFrame getSelectedFrame() {
        return ourFrameManager.getCurrent();
    }
    
    public LispWindow getSelectedWindow() {
        return ourWindowManager.getCurrent();
    }

    public boolean isFrameAlive (LispFrame frame) {
        return ourFrameManager.isAlive(frame);
    }

    public List<LispFrame> getVisibleFrames () {
        return ourFrameManager.getVisibleFrames();
    }

    public List<LispFrame> getVisibleAndIconifiedFrames () {
        return ourFrameManager.getVisibleAndIconifiedFrames();
    }

    public List<LispFrame> getAllFrames () {
        return ourFrameManager.getData();
    }
    
    public LispFrame[] getBufferFrames(LispBuffer buffer) {
        return ourWindowManager.getBufferFrames(buffer);
    }

    public LispWindow getBufferWindowOnFrame(LispFrame frame, LispBuffer buffer) {
        return ourWindowManager.getBufferWindowOnFrame(frame, buffer);
    }

    public void deleteFrameOtherWindows (LispFrame frame, LispWindow window) {
        ourWindowManager.deleteFrameOtherWindows(frame, window);
    }
    
    public boolean isWindowAlive (LispWindow window) {
        return ourWindowManager.isAlive(window);
    }

    @Nullable
    public LispMinibuffer getFrameMinibuffer (LispFrame frame) {
        LispWindow window = ourWindowManager.getFrameMinibufferWindow(frame);
        if (window == null)
            return null;
//            throw new NoFrameMinibuffer(frame.toString());
        return (LispMinibuffer) window.getBuffer();
    }

    public LispWindow getFrameMinibufferWindow (LispFrame frame) {
        return ourWindowManager.getFrameMinibufferWindow(frame);
    }

    public LispWindow getMinibufferWindow () {
        return getFrameMinibufferWindow(getSelectedFrame());
    }

    public List<LispWindow> getFrameWindows (LispFrame frame) {
        return ourWindowManager.getFrameWindows(frame);
    }

    public LispBuffer[] getFrameBuffers(LispFrame frame) {
        return ourWindowManager.getFrameBuffers(frame);
    }

    @NotNull
    public LispWindow getFrameSelectedWindow (LispFrame frame) {
        return ourWindowManager.getFrameSelectedWindow(frame);
    }

    //upload
    public LispSymbol findAndRegisterEmacsVariable (String name) {
        return DefinitionLoader.findAndRegisterEmacsVariable(name);
    }

    public LispSymbol findAndRegisterEmacsFunction (String name) {
        return DefinitionLoader.findAndRegisterEmacsFunction(name);
    }

    public LispList getEmacsDefFromFile (String name, String file, LispObject typeObject) {
//        if (typeObject.equals(LispSymbol.ourNil) || typeObject.equals(new LispSymbol("macro"))) //function or macro
        if (typeObject.equals(new LispSymbol("keymap")))
            throw new NotImplementedException("keymap autoload");
        if (file.endsWith(".elc"))
            throw new NotImplementedException("Cannot load from Emacs byte-compiled files!");
        if (!file.endsWith(".el"))
            file += ".el";
        return DefinitionLoader.getDefFromFile(file, name, DefinitionLoader.DefType.FUN);
    }

    //syntax tables
    public void setSyntaxTable (LispSyntaxTable table) {
        getBufferCurrentForEditing().setSyntaxTable(table);
    }

    public LispSyntaxTable getSyntaxTable () {
        return getBufferCurrentForEditing().getSyntaxTable();
    }
}


