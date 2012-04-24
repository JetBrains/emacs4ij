package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/17/11
 * Time: 1:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaWindow implements LispWindow {
//    private int myId;
//    private LispBuffer myBuffer;
    private Editor myEditor = null;
    private final String myString;

    public IdeaWindow (int id, String bufferName, Editor editor) {
        myString = "#<window " + id + " on " + bufferName + '>';
        set(editor);
    }

    public void set (@Nullable Editor editor) {
        myEditor = editor;
    }

    public Editor getEditor() {
        return myEditor;
    }

    @Override
    public String toString() {
        return myString;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    public int getSize() {
        return myEditor.getDocument().getTextLength();
    }

    public int point() {
        return myEditor.logicalPositionToOffset(myEditor.getCaretModel().getLogicalPosition()) + 1;
    }

    @Override
    public int followingCharacter() {
        if (point() == pointMax())
            return 0;
        return (int)myEditor.getDocument().getText().charAt(point()-1);
    }

    @Override
    public int precedingCharacter() {
        if (point() == pointMin())
            return 0;
        return (int)myEditor.getDocument().getText().charAt(point()-2);
    }

    public void setPoint(int position) {
        myEditor.getCaretModel().moveToOffset(position);
    }

    public int pointMin() {
        return 1;
    }

    public int pointMax() {
        return getSize()+1;
    }

    public String gotoChar (int position) {
        String message = "";
        if (position < pointMin()) {
            position = pointMin();
            message = "Beginning of buffer";
        }
        else if (position > pointMax()) {
            position = pointMax();
            message = "End of buffer";
        }
        myEditor.getCaretModel().moveToOffset(position-1);
        return message;
    }

    public void write (final String text) {
        if (myEditor == null)
            return;
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        myEditor.getDocument().setText(text);
                        gotoChar(pointMax());
                    }
                });
            }
        });
    }

    public void insertAt (final int position, final String insertion) {
        if (myEditor == null)
            return;
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        myEditor.getDocument().insertString(position, insertion);
                    }
                });
            }
        });
    }

    public void closeHeader () {
        if (myEditor.getHeaderComponent() == null)
            return;
        myEditor.setHeaderComponent(null);
    }

    public boolean hasEditor() {
        return myEditor != null;
    }
}
