package org.jetbrains.emacs4ij;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
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
    public String toString() {
        return "#<window " + myId + " on " + myBuffer.getName() + '>';
    }

    @Override
    public LObject evaluate(Environment environment) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LObject invokeMethod(String methodName, Class[] parameterTypes, Object... methodParameters) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
