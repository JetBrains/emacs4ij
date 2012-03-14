package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/1/12
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispArray {
    void setItem (int position, LObject value);
    LObject getItem (int position);
}
