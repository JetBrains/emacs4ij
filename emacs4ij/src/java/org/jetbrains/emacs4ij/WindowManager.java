package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.emacs4ij.jelisp.elisp.LispWindow;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.NoEditorException;
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
public class WindowManager {
    private List<LispWindow> myWindows = new ArrayList<>();
    private LispWindow mySelectedWindow = null;

    public WindowManager() {}

    public void setActiveEditor (Editor editor, String bufferName) {
        if (mySelectedWindow == null) {
            if (editor != null)
                add(editor, bufferName);
            return;
        }
        if (myWindows.size() != 1)
            throw new InternalException(Emacs4ijBundle.message("reset.not.single.editor"));
        mySelectedWindow.set(editor);
    }

    public Document getDocument () {
        if (myWindows.isEmpty())
            return null;
        return myWindows.get(0).getEditor().getDocument();
    }
    
    public void switchToEditor (Editor editor) {
        LispWindow my = getByEditor(editor);
        if (my == null)
            throw new UnregisteredEditorException();
        mySelectedWindow = my;
    }

    private LispWindow getByEditor (Editor editor) {
        for (LispWindow window: myWindows) {
            if (window.getEditor() == editor)
                return window;
        }
        return null;
    }
    
    public LispWindow getSelectedWindow() {
        if (mySelectedWindow == null)
            throw new NoEditorException();
        return mySelectedWindow;
    }
    
    public void add (Editor editor, String bufferName) {
        LispWindow bufferEditor = new IdeaWindow(myWindows.size(), bufferName, editor);
        myWindows.add(bufferEditor);
        mySelectedWindow = bufferEditor;
    }

    public void closeAll() {
        mySelectedWindow = null;
        myWindows.clear();
    }

    public boolean contains (Editor editor) {
        for (LispWindow window: myWindows) {
            if (window.getEditor() == editor)
                return true;
        }
        return false;
    }

    public void add (FileEditorManager fileEditorManager, VirtualFile file) {
        FileEditor selected = fileEditorManager.getSelectedEditor(file);
        for (FileEditor fileEditor: fileEditorManager.getAllEditors(file)) {
            LispWindow window = new IdeaWindow(myWindows.size(), file.getName(), ((TextEditor)fileEditor).getEditor());
            myWindows.add(window);
            if (file == selected)
                mySelectedWindow = window;
        }
        if (myWindows.isEmpty())
            throw new InternalException(Emacs4ijBundle.message("file.with.no.editors", file.getName()));
    }

    public boolean isEmpty() {
        return myWindows.isEmpty();
    }
}
