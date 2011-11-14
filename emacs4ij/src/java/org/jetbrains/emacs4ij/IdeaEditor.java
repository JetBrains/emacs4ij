package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.impl.EditorImpl;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMiniBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;

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

    private static Project ourProject;

    //buffer-local elisp variables
    private String myDefaultDirectory;
    private boolean isAlive;

    protected IdeaEditor() {}

    public IdeaEditor (Environment environment, String name, String path, Editor editor) {
        myEnvironment = environment;
        myName = name;
        myEditor = editor;
        myDefaultDirectory = path;
        isAlive = true;
    }

    public static void setProject(Project project) {
        ourProject = project;
    }

    public void kill () {
        isAlive = false;
        close();
    }

    public boolean isAlive() {
        return isAlive;
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

    protected void write (final String text) {
        if (myEditor == null)
            return;
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                myEditor.getDocument().setText(text);
                gotoChar(pointMax());
            }
        });
    }

    protected void setHeaderBufferActive () {
        if (myEnvironment.getServiceBuffer(myName) == null)
            throw new NoBufferException(myName);
        if (myEditor == null)
            throw new RuntimeException("null editor!");
        if (!(this instanceof IdeaMiniBuffer))
            write("");
        myEnvironment.updateServiceBuffer(this);
        myEditor.getContentComponent().grabFocus();
    }

    private boolean isHeaderBuffer () {
        return (myName.equals(GlobalEnvironment.ourScratchBufferName) || this instanceof LispMiniBuffer);
    }

    @Override
    public void setBufferActive () {
        if (isHeaderBuffer()) {
            setHeaderBufferActive();
            return;
        }
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
        VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
        for (VirtualFile file: openedFiles) {
            if (file.getName().equals(myName)) {
                fileEditorManager.openTextEditor(new OpenFileDescriptor(ourProject, file), true);
            }
        }
    }

    @Override
    public String getDefaultDirectory() {
        return myDefaultDirectory;
    }

    @Override
    public void closeHeader () {
        if (myEditor.getHeaderComponent() == null)
            return;
        myEditor.setHeaderComponent(null);
        myEnvironment.updateBuffer(this);
    }

    public void close () {
        if (isHeaderBuffer()) {
            LispBuffer displayedBuffer = myEnvironment.findBuffer(getDisplayedBufferName());
            displayedBuffer.closeHeader();
            return;
        }
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
        VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
        for (VirtualFile file: openedFiles) {
            if (file.getName().equals(myName)) {
                fileEditorManager.closeFile(file);
            }
        }
    }

    public static String getDisplayedBufferName () {
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
        try {
            return ((EditorImpl)fileEditorManager.getSelectedTextEditor()).getVirtualFile().getName();
        } catch (NullPointerException e) {
            throw new RuntimeException(e);
        }
    }
}
