package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindowFactory;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 3:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class WindowCreator implements LispWindowFactory {
    @Override
    public LispWindow createWindow(int id, LispBuffer buffer, LispFrame frame, Editor editor) {
        return new IdeaWindow(id, buffer, frame, editor);
    }
}
