package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.interactive.InteractiveReader;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 3:01 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispMiniBuffer extends LispBuffer {
    void readParameter(InteractiveReader interactive);
    void addCharListener();
    int getActivationsDepth();
    void setInputStartValue (String startValue);
    void updateEditorText();

    void onReadInput();
    void onInteractiveNoIoInput(InteractiveReader interactive);

    //todo: as in emacs
    String readInputString();

    //it's for completer interface
    List<String> getCompletions (String parameter);
    void setNoMatch(String input);
}
