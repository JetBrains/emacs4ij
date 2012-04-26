package org.jetbrains.emacs4ij;

import com.intellij.openapi.wm.IdeFrame;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.FrameManager;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.exception.NoLispFrameForIdeFrame;

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

    @Override
    public void onFrameOpened(LispFrame frame) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void onFrameReleased(LispFrame frame) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void bury(LispFrame item) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean define(LispFrame item) {
        assert myFrame == item;
        return true;
    }

    @NotNull
    @Override
    public LispFrame getCurrent() {
        return myFrame;
    }

    @Override
    public LispFrame switchTo(LispFrame item) {
        assert myFrame == item;
        return myFrame;
    }

    @Override
    public boolean isAlive(LispFrame frame) {
        return frame == myFrame;
    }

    @Override
    public boolean isEmpty() {
        return false;
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
    public LispFrame getFrame(IdeFrame frame) {
        if (myFrame.getIdeFrame() == frame)
            return myFrame;
        throw new NoLispFrameForIdeFrame();
    }

    @Override
    public List<LispFrame> getData() {
        return Arrays.asList(myFrame);
    }

    @Override
    public int getSize() {
        return 1;
    }

    @Override
    public void remove(LispFrame item) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }
}
