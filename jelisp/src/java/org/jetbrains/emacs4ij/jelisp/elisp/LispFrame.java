package org.jetbrains.emacs4ij.jelisp.elisp;

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
    void setVisible (boolean visible);
    boolean isVisible ();
    void setIconified (boolean iconified);
    boolean isIconified ();

    boolean areIdeFramesEqual (LispFrame frame);

    void openWindow (LispBuffer buffer);
    LispWindow containsBuffer (LispBuffer buffer);
    void closeWindow (LispBuffer buffer);
    List<LispBuffer> getBufferList();
    JComponent getComponent();
}
