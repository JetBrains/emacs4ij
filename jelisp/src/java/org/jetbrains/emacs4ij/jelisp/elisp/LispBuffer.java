package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.annotations.Nullable;

import java.util.List;

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
    void closeHeader();

    void addMarker (LispMarker marker);
    void removeMarker (LispMarker marker);
    boolean hasMarkersAt (int position);
    LispMarker getMark();
    void setMark (LispMarker mark);

    void insert(String insertion);
    void insert(String insertion, int where);
    void insert(LispObject insertion, @Nullable LispMarker where);
    void insert(LispObject insertion);

    void setEditor(Editor editor);
    void switchToEditor (Editor editor);
    Editor getEditor();
    LispWindow getSelectedWindow();
    boolean containsEditor (Editor editor);
    boolean hasEditors();
    Document getDocument();
    List<LispWindow> getWindows();
    void mergeEditors (LispBuffer other);
}
