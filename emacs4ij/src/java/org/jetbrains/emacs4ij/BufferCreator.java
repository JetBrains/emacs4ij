package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBufferFactory;
import org.jetbrains.emacs4ij.jelisp.elisp.LispToolWindow;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 7:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferCreator implements LispBufferFactory {
    @Override
    public LispBuffer createBuffer(Environment environment, @NotNull VirtualFile file, @Nullable Editor editor) {
        return new IdeaBuffer(environment, file, editor);
    }

    @Override
    public LispBuffer createBuffer(Environment environment, String name, String defaultDir, LispToolWindow window) {
        return new IdeaBuffer(environment, name, defaultDir, window);
    }
}
