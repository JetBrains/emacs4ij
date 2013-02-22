package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/30/12
 * Time: 11:14 AM
 * To change this template use File | Settings | File Templates.
 */
public interface MarkerOrInteger extends LispObject {
    LispBuffer getBuffer(Environment environment);
    Integer getPosition();
}
