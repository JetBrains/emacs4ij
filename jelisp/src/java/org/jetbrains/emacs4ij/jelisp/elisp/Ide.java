package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/24/11
 * Time: 3:44 PM
 * To change this template use File | Settings | File Templates.
 */
public interface Ide {
    void echo(String message, GlobalEnvironment.MessageType type);
}
