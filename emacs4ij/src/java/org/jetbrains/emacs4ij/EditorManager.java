package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredEditorException;

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
            if (editor != null)
                add(editor);
            return;
        }
        if (myEditors.size() != 1)
            throw new InternalException(Emacs4ijBundle.message("reset.not.single.editor"));
        myCurrentEditor.set(editor);
    }
    
    public void switchToEditor (Editor editor) {
        BufferEditor my = getByEditor(editor);
        if (my == null)
            throw new UnregisteredEditorException();
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
            throw new InternalException(Emacs4ijBundle.message("no.editor.for.buffer"));
        return myCurrentEditor;
    }
    
    public void add (Editor editor) {
        BufferEditor bufferEditor = new BufferEditor(editor);
        myEditors.add(bufferEditor);
        myCurrentEditor = bufferEditor;
    }

    public void closeAll() {
        myCurrentEditor = null;
        myEditors.clear();
    }

    public boolean contains (Editor editor) {
        for (BufferEditor bufferEditor: myEditors) {
            if (bufferEditor.getEditor() == editor)
                return true;
        }
        return false;
    }

    public void add (FileEditorManager fileEditorManager, VirtualFile file) {
        FileEditor selected = fileEditorManager.getSelectedEditor(file);
        for (FileEditor fileEditor: fileEditorManager.getAllEditors(file)) {
            BufferEditor bufferEditor = new BufferEditor(((TextEditor)fileEditor).getEditor());
            myEditors.add(bufferEditor);
            if (file == selected)
                myCurrentEditor = bufferEditor;
        }
    }
}
