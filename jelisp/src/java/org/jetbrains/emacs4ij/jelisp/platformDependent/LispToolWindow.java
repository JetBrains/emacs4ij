package org.jetbrains.emacs4ij.jelisp.platformDependent;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 6/1/12
 * Time: 3:34 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispToolWindow extends LispWindow {
    void setId (int id);
    void setFrame (LispFrame frame);
    boolean isRegistered();
    EditorWrapper getEditor();
}
