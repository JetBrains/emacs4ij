package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMiniBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/3/12
 * Time: 2:10 PM
 * To change this template use File | Settings | File Templates.
 */
public interface BufferManager {
    boolean defineBuffer (LispBuffer buffer);
    void defineServiceBuffer (LispBuffer buffer);

    List<String> getBuffersNames(String begin);
    List<String> getBuffersNames();
    void defineBufferLocalVariable(LispSymbol var);
    void closeCurrentBuffer();
    void killBuffer (LispBuffer buffer);
    LispBuffer getCurrentBuffer();
    LispBuffer getOtherBuffer (String name);
    LispBuffer getServiceBuffer (String name);
    LispBuffer createBuffer (String name);
    LispBuffer switchToWindow (String bufferName, Editor editor);
    void switchToWindow (LispWindow window);
    LispBuffer switchToBuffer (String bufferName);

    List<LispBuffer> getBuffers();
    int getBuffersSize();
    void closeAllBuffers();
    LispBuffer getBufferByIndex(int index);
    boolean isDead(String bufferName);
    boolean containsBuffer (String bufferName);
    boolean containsWindow(LispWindow window);
    LispBuffer findBufferSafe(String bufferName);
    LispBuffer findBuffer(String bufferName);
    LispBuffer findBuffer(Editor editor);
    LispBuffer lastBuffer (String bufferName);
    void buryBuffer (LispBuffer buffer);
    void removeBuffer (LispBuffer buffer);

    LispMiniBuffer getMinibuffer();
    List<LispWindow> getWindows();
}
