package org.jetbrains.emacs4ij.ide;

import org.jetbrains.emacs4ij.jelisp.platform_dependent.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 3:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class WindowCreator implements LispWindowFactory {
    @Override
    public LispWindow createWindow(int id, final LispBuffer buffer, final LispFrame frame, final EditorWrapper editor) {
        return new IdeaWindow(id, buffer, frame, ((IdeaEditorWrapper)editor).getEditor());
    }
}
