package org.jetbrains.emacs4ij;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.BufferManager;
import org.jetbrains.emacs4ij.jelisp.FrameManager;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;

import java.util.Arrays;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/3/12
 * Time: 6:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestFrameManagerImpl implements FrameManager {
    private LispFrame myFrame = new IdeaFrame();

    public TestFrameManagerImpl() {}

    @Override
    public void onFrameOpened(LispFrame frame) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void onFrameReleased(LispFrame frame) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setSelectedFrame(LispFrame frame) {
        throw new UnsupportedOperationException();
    }

    @Override
    public LispFrame getSelectedFrame() {
        return myFrame;
    }

    @Override
    public LispWindow getSelectedWindow() {
        return myFrame.getSelectedWindow();
    }

    @Override
    public void setFrameVisible(LispFrame frame, boolean status) {
        if (frame != myFrame)
            throw new UnsupportedOperationException();
        myFrame.setVisible(status);
    }

    @Override
    public void setFrameIconified(LispFrame frame, boolean status) {
        if (frame != myFrame)
            throw new UnsupportedOperationException();
        myFrame.setIconified(status);
    }

    @Override
    public boolean isFrameAlive(LispFrame frame) {
        return frame == myFrame;
    }

    @Override
    public void openServiceBuffer(LispBuffer buffer) {
        myFrame.getBufferManager().defineServiceBuffer(buffer);
    }

    @Override
    public void openBuffer(LispBuffer buffer) {
        myFrame.getBufferManager().defineBuffer(buffer);
    }

    @Override
    public List<LispFrame> getVisibleFrames() {
        return Arrays.asList(myFrame);
    }

    @Override
    public List<LispFrame> getVisibleAndIconifiedFrames() {
        return Arrays.asList(myFrame);
    }

    @Override
    public List<LispFrame> getAllFrames() {
        return Arrays.asList(myFrame);
    }

    @Override
    public List getBuffers() {
        return myFrame.getBufferManager().getBuffers();
    }

    @Override
    public List getBuffers(LispFrame frame) {
        if (frame != myFrame)
            throw new UnsupportedOperationException();
        return getBuffers();
    }

    @Override
    public void defineBufferLocalVariable(LispSymbol var) {
        myFrame.getBufferManager().defineBufferLocalVariable(var);
    }

    @Override
    public void killBuffer(@NotNull LispBuffer buffer) {
        myFrame.getBufferManager().killBuffer(buffer);
    }

    @Override
    public BufferManager getCurrentBufferManager() {
        return myFrame.getBufferManager();
    }

    @Override
    public LispFrame getFrameByWindow(LispWindow window) {
        if (myFrame.containsWindow(window))
            return myFrame;
        return null;
    }

    @Override
    public List<LispFrame> getFramesByBuffer(LispBuffer buffer) {
        if (myFrame.getBufferManager().containsBuffer(buffer.getName()))
            return Arrays.asList(myFrame);
        return null;
    }
}
