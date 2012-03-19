package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/19/12
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispCommand extends LispObject {
    boolean isInteractive();
    String getInteractiveString();
    LispList getInteractiveForm();
}
