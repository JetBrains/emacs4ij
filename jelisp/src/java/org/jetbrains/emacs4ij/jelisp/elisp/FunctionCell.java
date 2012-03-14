package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/19/11
 * Time: 2:26 PM
 * To change this template use File | Settings | File Templates.
 */
public interface FunctionCell extends LispObject {
    LispObject getDocumentation();
    void setDocumentation (LispObject doc);
    boolean isInteractive();
    String getInteractiveString();
}
