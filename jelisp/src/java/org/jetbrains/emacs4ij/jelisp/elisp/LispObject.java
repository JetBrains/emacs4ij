package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 1:28 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class LispObject {
    //protected boolean myIsQuoted;
    public abstract LispString toLispString();
}
