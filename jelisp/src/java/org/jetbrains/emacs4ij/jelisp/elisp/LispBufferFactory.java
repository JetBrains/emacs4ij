package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/31/11
 * Time: 3:56 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBufferFactory {
    public LispBuffer createBuffer (String bufferName, Environment environment);
}
