package org.jetbrains.emacs4ij.jelisp.elisp;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/6/12
 * Time: 5:31 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispSequence {
    public int length();
    public List<LObject> toLObjectList();
}
