package org.jetbrains.emacs4ij;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/17/11
 * Time: 1:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaWindow implements LispWindow {
    private int myId;
    private LispBuffer myBuffer;

    public IdeaWindow (int id, LispBuffer buffer) {
        myId = id;
        myBuffer = buffer;
    }
    
    @Override
    public boolean containsBuffer(LispBuffer buffer) {
        return myBuffer == buffer;
    }

    @Override
    public LispBuffer getBuffer() {
        return myBuffer;
    }

    @Override
    public String toString() {
        return "#<window " + myId + " on " + myBuffer.getName() + '>';
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }
}
