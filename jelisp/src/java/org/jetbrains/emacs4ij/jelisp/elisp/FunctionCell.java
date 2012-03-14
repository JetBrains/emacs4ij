package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/19/11
 * Time: 2:26 PM
 * To change this template use File | Settings | File Templates.
 */
public interface FunctionCell extends LObject {
    LObject getDocumentation();
    void setDocumentation (LObject doc);
    boolean isInteractive();
    String getInteractiveString();
}
