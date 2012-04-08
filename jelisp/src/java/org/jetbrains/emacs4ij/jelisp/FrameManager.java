package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/3/12
 * Time: 11:41 AM
 * To change this template use File | Settings | File Templates.
 */
public interface FrameManager {
    void onFrameOpened (LispFrame frame);
    void onFrameReleased (LispFrame frame);
    void setSelectedFrame (LispFrame frame);
    LispFrame getSelectedFrame();
    void setFrameVisible   (LispFrame frame, boolean status);
    void setFrameIconified (LispFrame frame, boolean status);
    boolean isFrameAlive (LispFrame frame);

    void openServiceBuffer(LispBuffer buffer);
    void openBuffer(LispBuffer buffer);

    List<LispFrame> getVisibleFrames();
    List<LispFrame> getVisibleAndIconifiedFrames();
    List<LispFrame> getAllFrames();

    List getBuffers();
    List getBuffers(LispFrame frame);
    void defineBufferLocalVariable (LispSymbol var);
    void killBuffer (@NotNull LispBuffer buffer);

    BufferManager getCurrentBufferManager();

    LispWindow getSelectedWindow();
}
