package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.wm.IdeFrame;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispFrame extends LispObject {
//    LispObject getParameter (String parameter);
//    void setParameter(String name, LispObject value);
    LispObject getParameter (LispSymbol parameter);
    void setParameter(LispSymbol name, LispObject value);
    void setVisible (boolean visible);
    boolean isVisible ();
    void setIconified (boolean iconified);
    boolean isIconified ();
    IdeFrame getIdeFrame();
    JComponent getComponent();
    LispList getParameters();
}
