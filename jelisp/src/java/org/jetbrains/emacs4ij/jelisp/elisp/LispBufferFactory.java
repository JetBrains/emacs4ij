package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 7:28 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBufferFactory {
    LispBuffer createBuffer (Environment environment, VirtualFile file, @Nullable Editor editor);
    LispBuffer createBuffer (Environment environment, String name, String defaultDir, LispToolWindow window);
}
