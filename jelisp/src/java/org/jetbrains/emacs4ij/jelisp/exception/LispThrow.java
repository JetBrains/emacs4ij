package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.elisp.LObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/22/12
 * Time: 11:16 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispThrow extends LispException {
    private LObject myTag;
    private LObject myValue;

    public LispThrow (LObject tag, LObject value) {
        myTag = tag;
        myValue = value;
    }

    public LObject getTag() {
        return myTag;
    }

    public LObject getValue() {
        return myValue;
    }
}
