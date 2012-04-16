package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.annotations.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/17/11
 * Time: 12:33 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispWindow extends LispObject {
//    boolean containsBuffer (LispBuffer buffer);
//    LispBuffer getBuffer();
    void set (@Nullable Editor editor);
    Editor getEditor();

    void closeHeader();
    int getSize();
    int point();
    int followingCharacter();
    int precedingCharacter();
    int pointMax();
    int pointMin();
    void setPoint(int position);
    String gotoChar (int position);
    void write(String text);
    void insertAt(int position, String text);
    boolean hasEditor();
}
