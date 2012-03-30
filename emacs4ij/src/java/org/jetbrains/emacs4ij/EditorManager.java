package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/30/12
 * Time: 4:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class EditorManager {
    private List<BufferEditor> myEditors = new ArrayList<>();
    private BufferEditor myCurrentEditor = null;

    public EditorManager() {}

    public void setActiveEditor (Editor editor) {
        if (myCurrentEditor == null) {
            add(editor);
            return;
        }
        myCurrentEditor.set(editor);
    }
    
    public void switchToEditor (Editor editor) {
        BufferEditor my = getByEditor(editor);
        if (my == null)
            throw new InternalException("Buffer doesn't have such registered editor!");
        myCurrentEditor = my;
    }

    private BufferEditor getByEditor (Editor editor) {
        for (BufferEditor bufferEditor: myEditors) {
            if (bufferEditor.getEditor() == editor)
                return bufferEditor;            
        }
        return null;
    }
    
    public BufferEditor getActiveEditor () {
        if (myCurrentEditor == null)
            throw new InternalException("No current editor for current buffer!");
        return myCurrentEditor;
    }
    
    public void add (Editor editor) {
        BufferEditor bufferEditor = new BufferEditor(editor);
        myEditors.add(bufferEditor);
        myCurrentEditor = bufferEditor;
    }
}
