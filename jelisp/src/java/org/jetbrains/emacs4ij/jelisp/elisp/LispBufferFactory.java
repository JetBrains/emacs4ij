package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/31/11
 * Time: 3:56 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBufferFactory {
    LispBuffer createBuffer (String bufferName);
}
