package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/19/11
 * Time: 2:26 PM
 * To change this template use File | Settings | File Templates.
 */
public interface FunctionCell extends LObject {
    public LObject getDocumentation();
    public void setDocumentation (LObject doc);
    public boolean isInteractive();
    public String getInteractiveString();
}
