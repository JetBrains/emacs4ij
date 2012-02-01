package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

import java.util.Collections;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/19/11
 * Time: 2:16 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BufferUtil {
    public static void setActiveBuffer (List<String> frameBufferNames, String bufferName) {
        setFirst(frameBufferNames, bufferName);
        setFirst(GlobalEnvironment.INSTANCE.getBufferNamesList(""), bufferName);
    }

    private static void setFirst (List<String> bufferNames, String bufferName) {
        if (bufferNames.size() == 0)
            return;
        if (bufferNames.get(bufferNames.size() - 1).equals(bufferName))
            return;
        int newCurrentBufferIndex = bufferNames.indexOf(bufferName);
        if (newCurrentBufferIndex == -1)
            throw new RuntimeException("Buffer " + bufferName + " is not opened");
        Collections.rotate(bufferNames.subList(newCurrentBufferIndex, bufferNames.size()), -1);
    }
}
