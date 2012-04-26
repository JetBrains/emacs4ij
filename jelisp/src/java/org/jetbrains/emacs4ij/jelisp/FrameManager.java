package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.wm.IdeFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/3/12
 * Time: 11:41 AM
 * To change this template use File | Settings | File Templates.
 */
public interface FrameManager extends Manager<LispFrame> {
    void onFrameOpened (LispFrame frame);
    void onFrameReleased (LispFrame frame);
    List<LispFrame> getVisibleFrames();
    List<LispFrame> getVisibleAndIconifiedFrames();
    LispFrame getFrame (IdeFrame frame);
}
