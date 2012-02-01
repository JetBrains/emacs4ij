package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/24/11
 * Time: 3:44 PM
 * To change this template use File | Settings | File Templates.
 */
public interface Ide {
    public void showErrorMessage (String message);
    public void showMessage (String message);
}
