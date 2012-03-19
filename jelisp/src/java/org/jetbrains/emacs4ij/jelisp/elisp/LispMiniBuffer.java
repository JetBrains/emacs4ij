package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;

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
    void open(Editor parent);

    //todo: these methods mustn't return the evaluation result. This is only for testing.
    LispObject onReadInput();
    LispObject onInteractiveNoIoInput(SpecialFormInteractive interactive);
    LispObject onInteractiveCall(Environment environment, LispSymbol command);
}
