package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.EditorWrapper;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 7/3/12
 * Time: 1:45 PM
 * To change this template use File | Settings | File Templates.
 */
public final class IdeaEditorWrapper implements EditorWrapper {
    private final Editor myEditor;

    IdeaEditorWrapper (final Editor editor) {
        myEditor = editor;
    }

    Editor getEditor() {
        return myEditor;
    }
}
