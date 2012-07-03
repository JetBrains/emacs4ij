package org.jetbrains.emacs4ij;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispBufferFactory;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispToolWindow;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 7:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferCreator implements LispBufferFactory {
//    @Override
//    public LispBuffer createBuffer(Environment environment, @NotNull VirtualFile file, @Nullable Editor editor) {
//        return new IdeaBuffer(environment, file, editor);
//    }

    @Override
    public LispBuffer createBuffer(Environment environment, String name, String defaultDir, LispToolWindow window) {
        return new IdeaBuffer(environment, name, defaultDir, window);
    }
}
