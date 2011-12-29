package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispFrame extends LObject {
    public LObject getParameter (String parameter);
    public void setVisible (boolean visible);
    public boolean isVisible ();
    public void setIconified (boolean iconified);
    public boolean isIconified ();

    public boolean areIdeFramesEqual (LispFrame frame);

    public void openWindow (LispBuffer buffer);
    public LispWindow containsBuffer (LispBuffer buffer);
    public void closeWindow (LispBuffer buffer);
    public List<LispBuffer> getBufferList();
}
