package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.BufferManager;

import javax.swing.*;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispFrame extends LispObject {
    LispObject getParameter (String parameter);
    void setParameter(String name, LispObject value);
    void setVisible (boolean visible);
    boolean isVisible ();
    void setIconified (boolean iconified);
    boolean isIconified ();
    boolean areIdeFramesEqual (LispFrame frame);

    void openWindow (LispBuffer buffer);
    void openServiceWindow (LispBuffer buffer);
    LispWindow getSelectedWindow();
    LispWindow getBufferWindow (LispBuffer buffer);
    LispBuffer getWindowBuffer (LispWindow window);
    void closeWindow (LispBuffer buffer);

    BufferManager getBufferManager();

    boolean containsWindow (LispWindow window);

    LispMiniBuffer getMinibuffer();
    List<LispWindow> getWindows();
    void deleteOtherWindows (LispWindow window);

    JComponent getComponent();
}
