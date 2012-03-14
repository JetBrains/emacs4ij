package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/17/11
 * Time: 12:33 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispWindow extends LispObject {
    boolean containsBuffer (LispBuffer buffer);
    LispBuffer getBuffer();
}
