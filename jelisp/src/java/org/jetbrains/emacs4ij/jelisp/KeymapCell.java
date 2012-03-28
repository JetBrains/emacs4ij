package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/19/12
 * Time: 3:47 PM
 * To change this template use File | Settings | File Templates.
 */
public interface KeymapCell extends LispObject {
    LispKeymap getKeymap();
}
