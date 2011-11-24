package org.jetbrains.emacs4ij;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBufferFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/31/11
 * Time: 4:30 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferCreator implements LispBufferFactory {

    public BufferCreator () {}

    @Override
    public LispBuffer createBuffer(String bufferName, Environment environment) {
        //TODO The major mode for a newly created buffer is set to Fundamental mode.
        // (The default value of the variable major-mode is handled at a higher level; see Auto Major Mode.)
        // If the name begins with a space, the buffer initially disables undo information recording (see Undo).
        // http://www.gnu.org/software/emacs/elisp/html_node/Creating-Buffers.html#Creating-Buffers

        GlobalEnvironment global = GlobalEnvironment.getInstance();
        String baseDir = ((LispString)global.getBufferCurrentForEditing().getLocalVariableValue("directory")).getData();
        return new IdeaBuffer(global, bufferName, baseDir, null);
    }
}
