package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:19 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispBuffer extends LispObject {
    void onOpen (Document document);
    void reopen (Editor editor, VirtualFile file);
    LispObject getVariableValue (String name);
    LispSymbol getVariable (String name);
    void defineVariable (LispSymbol variable);
    boolean hasVariable (String variable);
    Map<LispSymbol, LispObject> getAllLocalVarValues();
    void killVariable (String name);
    void reset();
    Environment getEnvironment();

    String getName();

    int size();
    int point();
    void setPoint(int position);
    int pointMin();
    int pointMax();
    String gotoChar(int position);
    String forwardChar (int shift);
    int followingCharacter();
    int precedingCharacter();

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

    Editor getEditor();
    Document getDocument();

    void setSyntaxTable(LispSyntaxTable table);
    LispSyntaxTable getSyntaxTable ();

    LispString substring (int start, int end, boolean withProperties);
    void replace (int from, int to, String text);
}
