package org.jetbrains.emacs4ij.jelisp.platformDependent;

import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispFrame extends LispObject {
    LispObject getParameter (LispSymbol parameter);
    void setParameter(LispSymbol name, LispObject value);
    void setVisible (boolean visible);
    boolean isVisible ();
    void setIconified (boolean iconified);
    boolean isIconified ();
    JComponent getComponent();
    LispList getParameters();
}
