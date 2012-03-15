package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 4:31 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispStringOrVector extends LispObject {
    String toShortcutString();
}
