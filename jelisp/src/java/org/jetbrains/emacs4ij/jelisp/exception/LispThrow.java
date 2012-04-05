package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/22/12
 * Time: 11:16 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispThrow extends LispException {
    private LispObject myTag;
    private LispObject myValue;

    public LispThrow (LispObject tag, LispObject value) {
        myTag = tag;
        myValue = value;
    }

    public LispObject getTag() {
        return myTag;
    }

    public LispObject getValue() {
        return myValue;
    }

    @Override
    public String getMessage() {
        return JelispBundle.message("lisp.throw", myTag.toString(), myValue.toString());
    }
}
