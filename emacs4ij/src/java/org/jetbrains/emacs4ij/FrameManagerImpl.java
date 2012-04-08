package org.jetbrains.emacs4ij;

import org.apache.commons.collections.ListUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.BufferManager;
import org.jetbrains.emacs4ij.jelisp.FrameManager;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredFrameException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/3/12
 * Time: 11:42 AM
 * To change this template use File | Settings | File Templates.
 */
public class FrameManagerImpl implements FrameManager {
    private List<LispFrame> myFrames = new ArrayList<>();
    private LispFrame myCurrentFrame = null;

    public FrameManagerImpl () {}

    @Override
    public void onFrameOpened(LispFrame frame) {
        if (frameIndex(frame) != -1)
            return;
        myFrames.add(frame);
    }

    @Override
    public BufferManager getCurrentBufferManager() {
        if (myCurrentFrame == null)
            throw new InternalException(Emacs4ijBundle.message("no.frame"));
        return myCurrentFrame.getBufferManager();
    }

    @Override
    public void openServiceBuffer(LispBuffer buffer) {
        if (myCurrentFrame != null)
            myCurrentFrame.openServiceWindow(buffer);
    }

    @Override
    public void openBuffer(LispBuffer buffer) {
        if (myCurrentFrame != null)
            myCurrentFrame.openWindow(buffer);
    }

    @Override
    public void onFrameReleased (LispFrame frame) {
        myFrames.remove(frame);
    }

    @Override
    public void setSelectedFrame (LispFrame frame) {
        int k = frameIndex(frame);
        if (k < 0)
            throw new UnregisteredFrameException(frame);
        myCurrentFrame = myFrames.get(k);
    }

    @Override
    public LispFrame getSelectedFrame() {
        return myCurrentFrame;
    }

    private int frameIndex (LispFrame frame) {
        for (int i = 0; i != myFrames.size(); ++i) {
            if (myFrames.get(i).areIdeFramesEqual(frame))
                return i;
        }
        return -1;
    }

    @Override
    public void setFrameVisible (LispFrame frame, boolean status) {
        myFrames.get(frameIndex(frame)).setVisible(status);
    }

    @Override
    public void setFrameIconified (LispFrame frame, boolean status) {
        myFrames.get(frameIndex(frame)).setIconified(status);
    }

    @Override
    public boolean isFrameAlive (LispFrame frame) {
        return frameIndex(frame) >= 0;
    }

    @Override
    public List<LispFrame> getVisibleFrames () {
        List<LispFrame> visibleFrames = new ArrayList<>();
        for (LispFrame frame: myFrames) {
            if (BuiltinPredicates.frameVisibleP(GlobalEnvironment.INSTANCE, frame).equals(LispSymbol.ourT))
                visibleFrames.add(frame);
        }
        return visibleFrames;
    }

    @Override
    public List<LispFrame> getVisibleAndIconifiedFrames () {
        List<LispFrame> frames = new ArrayList<>();
        for (LispFrame frame: myFrames) {
            LispSymbol predicate = BuiltinPredicates.frameVisibleP(GlobalEnvironment.INSTANCE, frame);
            if (predicate.equals(LispSymbol.ourT) || predicate.equals(new LispSymbol("icon")))
                frames.add(frame);
        }
        return frames;
    }

    @Override
    public List<LispFrame> getAllFrames () {
        return myFrames;
    }

    @Override
    public LispWindow getSelectedWindow() {
        if (myCurrentFrame == null)
            return null;
        return myCurrentFrame.getSelectedWindow();
    }

    //== buffers ==
    @Override
    public List getBuffers(LispFrame lispFrame) {

        List bufferNamesList = lispFrame.getBufferManager().getBuffers();
        for (LispFrame frame: myFrames) {
            if (frame == lispFrame)
                continue;
            bufferNamesList = ListUtils.sum(bufferNamesList, frame.getBufferManager().getBuffers());
        }
        return bufferNamesList;
    }

    @Override
    public List getBuffers() {
        //todo: buffers must appear in order of most recent display, no matter frames order
        List bufferNamesList = new ArrayList<String>();
        for (LispFrame frame: myFrames) {
            bufferNamesList = ListUtils.sum(bufferNamesList, frame.getBufferManager().getBuffers());
        }
        return bufferNamesList;
    }

    @Override
    public void defineBufferLocalVariable(LispSymbol var) {
        for (LispFrame frame: myFrames) {
            frame.getBufferManager().defineBufferLocalVariable(var);
        }
    }

    @Override
    public void killBuffer(@NotNull LispBuffer buffer) {
        for (LispFrame frame: myFrames) {
            frame.getBufferManager().killBuffer(buffer);
        }
    }
}
