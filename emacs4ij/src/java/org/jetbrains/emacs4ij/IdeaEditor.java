package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.EditorTextField;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMiniBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

import java.awt.*;
import java.awt.event.KeyEvent;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaEditor extends LispObject implements LispBuffer {
    protected String myName;
    protected Editor myEditor;
    protected Environment myEnvironment;
    //buffer-local elisp variables
    private String myDefaultDirectory;

    protected IdeaEditor() {}

    public IdeaEditor (Environment environment, String name, String path, Editor editor) {
        myEnvironment = environment;
        myName = name;
        myEditor = editor;
        myDefaultDirectory = path;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IdeaEditor that = (IdeaEditor) o;

        if (myEditor != null ? !myEditor.equals(that.myEditor) : that.myEditor != null) return false;
        if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myName != null ? myName.hashCode() : 0;
        result = 31 * result + (myEditor != null ? myEditor.hashCode() : 0);
        return result;
    }

    public Editor getEditor() {
        return myEditor;
    }

    @Override
    public void setEditor(Editor editor) {
        myEditor = editor;
    }

    public String toString() {
        return "#<buffer " + myName + ">";
    }

    @Override
    public LObject evaluate(Environment environment) {
        throw new NotImplementedException();  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public String getName() {
        return myName;
    }

    @Override
    public int getSize() {
        return myEditor.getDocument().getTextLength();
    }

    @Override
    public int point() {
        return myEditor.logicalPositionToOffset(myEditor.getCaretModel().getLogicalPosition()) + 1;
    }

    @Override
    public int pointMin() {
        return 1;
    }

    @Override
    public int pointMax() {
        return getSize()+1;
    }

    @Override
    public int bufferEnd(double parameter) {
        return (parameter > 0) ? pointMax() : pointMin();
    }

    @Override
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

    @Override
    public String forwardChar (int shift) {
        return gotoChar(point() + shift);
    }

    protected void setHeaderBufferActive () {
        final EditorTextField input = new EditorTextField();
        LispBuffer buffer = myEnvironment.findBuffer(myName);
        if (buffer == null)
            throw new RuntimeException("buffer " + myName + " doesn't exist!");

        LispBuffer currentBuffer = myEnvironment.getBufferCurrentForEditing();
        if (currentBuffer.getName().equals(myName))
            return;

        final Editor editor = myEnvironment.getBufferCurrentForEditing().getEditor();

        if (EventQueue.isDispatchThread()) {
            ApplicationManager.getApplication().runReadAction(new Runnable() {
                @Override
                public void run() {
                    editor.setHeaderComponent(input);
                }
            });
        } else EventQueue.invokeLater(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runReadAction(new Runnable() {
                    @Override
                    public void run() {
                        editor.setHeaderComponent(input);
                    }
                });
            }
        });

       /* ApplicationManager.getApplication().runReadAction(new Runnable() {
            @Override
            public void run() {
                editor.setHeaderComponent(input);
            }
        });  */

        setEditor(input.getEditor());
        myEnvironment.updateBuffer(this);
        EvaluateCommand command = new EvaluateCommand();
        command.registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, input);
        grabFocus();
    }

    @Override
    public void grabFocus () {
        if (!((this instanceof LispMiniBuffer) || (myName.equals(OpenCommandEditor.ourScratch))))
            throw new RuntimeException("LispBuffer.grabFocus() wrong usage!");
        myEnvironment.switchToBuffer(myName);
        myEnvironment.printBuffers();
        if (myEditor == null)
            throw new RuntimeException("null editor!");
        myEditor.getContentComponent().grabFocus();
    }

    @Override
    public void setBufferActive () {
        if (myName.equals(OpenCommandEditor.ourScratch) || this instanceof LispMiniBuffer) {
            setHeaderBufferActive();
            return;
        }
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myEditor.getProject());
        VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
        for (VirtualFile file: openedFiles) {
            if (file.getName().equals(myName)) {
                fileEditorManager.openTextEditor(new OpenFileDescriptor(myEditor.getProject(), file), true);
            }
        }
    }

    @Override
    public String getDefaultDirectory() {
        return myDefaultDirectory;
    }
}
