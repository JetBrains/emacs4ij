package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.emacs4ij.jelisp.BufferManager;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import javax.swing.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaFrame implements LispFrame {
    private final IdeFrameImpl myFrame;
    private final FileEditorManager myFileEditorManager;
    private final BufferManager myBufferManager = new BufferManagerImpl();
    private Map<String, LispObject> myParameters = new HashMap<>();

    public IdeaFrame(IdeFrameImpl frame) {
        myFrame = frame;
        myParameters.put("visibility", LispSymbol.ourT);
        myFileEditorManager = FileEditorManager.getInstance(frame.getProject());
        initParameters();
    }

    //for test
    IdeaFrame () {
        myFrame = null;
        myFileEditorManager = null;
        initParameters();
    }

    private void initParameters() {
        setParameter("buffer-predicate", LispSymbol.ourNil);
    }

    @Override
    public String toString() {
        return "#<frame " + (myFrame == null ? "*test*" : myFrame.toString()) + '>';
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public LispObject getParameter(String parameter) {
        LispObject value = myParameters.get(parameter);
        if (value == null)
            throw new VoidVariableException(parameter);
        return value;
    }

    @Override
    public void setParameter(String name, LispObject value) {
        myParameters.put(name, value);
    }

    @Override
    public void setVisible(boolean visible) {
        myParameters.put("visibility", visible ? LispSymbol.ourT : LispSymbol.ourNil);
        myFrame.setVisible(visible);
    }

    @Override
    public void setIconified(boolean iconified) {
        myParameters.put("visibility", iconified ? new LispSymbol("icon") : LispSymbol.ourT);
        if (!iconified) {
            myFrame.show();
            //WindowManagerImpl.getInstance().
        }  else {
          //  myFrame.setFocusableWindowState(false);

           // myFrame.setVisible(false);
            // TODO: really iconify =)

        }
    }

    @Override
    public boolean isIconified() {
        return myParameters.get("visibility").equals(new LispSymbol("icon"));
    }

    @Override
    public boolean areIdeFramesEqual(LispFrame frame) {
        return myFrame.equals(((IdeaFrame)frame).myFrame);
    }

    @Override
    public boolean isVisible() {
        return myParameters.get("visibility").equals(LispSymbol.ourT);
    }

    @Override
    public void openServiceWindow (LispBuffer buffer) {
        myBufferManager.defineServiceBuffer(buffer);
    }

    @Override
    public void openWindow (LispBuffer buffer) {
        myBufferManager.defineBuffer(buffer);
    }

    @Override
    public LispWindow getSelectedWindow() {
        //todo: if focus not in editor?
        if (myFileEditorManager == null)
            return myBufferManager.getCurrentBuffer().getSelectedWindow();
        Editor e = myFileEditorManager.getSelectedTextEditor();
        return myBufferManager.findBuffer(myFileEditorManager.getSelectedTextEditor()).getSelectedWindow();
    }

    @Override
    public void closeWindow(LispBuffer buffer) {
        myBufferManager.removeBuffer(buffer);
    }

    @Override
    public BufferManager getBufferManager() {
        return myBufferManager;
    }

    @Override
    public LispWindow getBufferWindow (LispBuffer buffer) {
        LispBuffer my = myBufferManager.findBuffer(buffer.getName());
        if (my == null)
            return null;
        return my.getSelectedWindow();
    }

    @Override
    public JComponent getComponent() {
        return myFrame.getComponent();
    }

    @Override
    public boolean containsWindow(LispWindow window) {
        return myBufferManager.containsWindow(window);
    }

    @Override
    public LispMiniBuffer getMinibuffer() {
        return myBufferManager.getMinibuffer();
    }

    @Override
    public List<LispWindow> getWindows() {
        return myBufferManager.getWindows();
    }

    //    @Override
//    public List<LispBuffer> getBuffers() {
//        return myBufferManager.getBuffers();
//    }
//    @Override
//    public List<String> getBuffersNamesList(String begin) {
//        return myBufferManager.getBuffersNames(begin);
//    }
//
//    @Override
//    public List<String> getBuffersNamesList() {
//        return myBufferManager.getBuffersNames();
//    }
//
//
//    @Override
//    public void defineBufferLocalVariable(LispSymbol var) {
//        myBufferManager.defineBufferLocalVariable(var);
//    }
//
//    @Override
//    public void closeCurrentBuffer() {
//        closeWindow(myBufferManager.getCurrentBuffer());
//    }
//
//    @Override
//    public void closeBuffer(@NotNull LispBuffer buffer) {
//        closeWindow(buffer);
//    }
//
//    @Override
//    public void killBuffer(@NotNull LispBuffer buffer) {
//        myBufferManager.killBuffer(buffer);
//    }
//
//    @Override
//    public LispBuffer getCurrentBuffer() {
//        return myBufferManager.getCurrentBuffer();
//    }
//
//    @Override
//    public LispBuffer getOtherBuffer(String bufferName) {
//        return myBufferManager.getOtherBuffer(bufferName);
//    }
//
//    @Override
//    public LispBuffer createBuffer(String name) {
//        return myBufferManager.createBuffer(name);
//    }
//
//    @Override
//    public LispBuffer getServiceBuffer(String name) {
//        return myBufferManager.getServiceBuffer(name);
//    }
//
//    @Override
//    public LispBuffer switchToWindow(String bufferName, Editor editor) {
//        return myBufferManager.switchToWindow(bufferName, editor);
//    }
//
//    @Override
//    public LispBuffer switchToBuffer(String bufferName) {
//        return myBufferManager.switchToBuffer(bufferName);
//    }
}
