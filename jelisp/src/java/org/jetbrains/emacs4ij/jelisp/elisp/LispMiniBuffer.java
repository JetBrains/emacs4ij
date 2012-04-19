package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 3:01 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispMiniBuffer extends LispBuffer {
    void startRead();
    void readParameter(SpecialFormInteractive interactive);
    void addCharListener();
    int getActivationsDepth();
    void open(LispBuffer parent);
    boolean wasInteractiveFormResult();
    void setReadCommandStatus();
    void setInputStartValue (String startValue);
    void updateEditorText();

    LispObject onReadInput();
    LispObject onInteractiveNoIoInput(SpecialFormInteractive interactive);
    LispObject onInteractiveCall(Environment environment, LispSymbol command);

    //todo: as in emacs
    String readInputString();

    //it's for completer interface
    List<String> getCompletions (String parameter);
    void setNoMatch(String input);
}
