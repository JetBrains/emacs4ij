package org.jetbrains.emacs4ij.jelisp.platform_dependent;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispWindowFactory {
    LispWindow createWindow (int id, LispBuffer buffer, LispFrame frame, EditorWrapper editor);
}
