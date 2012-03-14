package org.jetbrains.emacs4ij.jelisp.elisp;

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

    //todo: these methods mustn't return the evaluation result. This is only for testing.
    LObject onReadInput();
    LObject onInteractiveNoIoInput(SpecialFormInteractive interactive);
}
