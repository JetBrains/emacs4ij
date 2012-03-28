package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBuffer extends LispObject {
    LispObject getLocalVariableValue (String name);
    LispSymbol getLocalVariable (String name);
    void defineLocalVariable (LispSymbol variable, boolean noValue);
    String getName();
    int getSize();
    int point();
    void setPoint(int position);
    int pointMin();
    int pointMax();
    String gotoChar(int position);
    String forwardChar (int shift);
    void kill();
    LispObject evaluateLastForm ();
    void setActive();
    Editor getEditor();
    void setEditor (Editor editor);
    void closeHeader();
    void addMarker (LispMarker marker);
    void removeMarker (LispMarker marker);
    boolean hasMarkersAt (int position);
    LispMarker getMark();
    void setMark (LispMarker mark);
}
