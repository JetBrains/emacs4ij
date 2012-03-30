package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/30/12
 * Time: 4:05 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferEditor {
    private Editor myEditor = null;
    private boolean isChangedByMe = false;
    private LispBuffer myBuffer;
    
    public BufferEditor (Editor editor) {
        set(editor);
    }
    
    protected final DocumentListener myDocumentListener = new DocumentListener() {
        private int myOldPosition;
        @Override
        public void beforeDocumentChange(DocumentEvent documentEvent) {
            myOldPosition = point();
        }

        @Override
        public void documentChanged(DocumentEvent documentEvent) {
            if (isChangedByMe) {
                isChangedByMe = false;
                return;
            }
            int shift = documentEvent.getNewLength() - documentEvent.getOldLength();
            if (shift < 0) {   //delete
//                myBuffer.updateMarkersPositions(point(), shift, false);
                return;
            }
            if (shift > 0) { //insert
//                myBuffer.updateMarkersPositions(myOldPosition, shift, false);
            }
        }
    };
    
    public void set (Editor editor) {
        if (myEditor != null) {
            myEditor.getDocument().removeDocumentListener(myDocumentListener);
        }
        myEditor = editor;
        if (myEditor != null) {
            myEditor.getDocument().addDocumentListener(myDocumentListener);
        }
    }

    public Editor getEditor() {
        return myEditor;
    }

    public int getSize() {
        return myEditor.getDocument().getTextLength();
    }

    public int point() {
        return myEditor.logicalPositionToOffset(myEditor.getCaretModel().getLogicalPosition()) + 1;
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
                        isChangedByMe = true;
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
