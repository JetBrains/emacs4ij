package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispWindowFactory {
    LispWindow createWindow (int id, LispBuffer buffer, LispFrame frame, Editor editor);
}
