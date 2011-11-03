package org.jetbrains.emacs4ij;

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
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaEditor extends LispObject implements LispBuffer {
    protected static ArrayList<IdeaEditor> openedHeaders = new ArrayList<IdeaEditor>();

    protected String myName;
    protected Editor myEditor;
    protected Environment myEnvironment;
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

    public void kill () {
        isAlive = false;
        close();
    }

    public boolean isAlive() {
        return isAlive;
    }

    @Override
    public Environment getEnvironment() {
        return myEnvironment;
    }

    public static void headerOpened (IdeaEditor header) {
        openedHeaders.add(header);
    }

    public static void headerClosed (IdeaEditor header) {
        for (LispBuffer buffer: header.getEnvironment().getBuffers()) {
            String headerName = buffer.getHeaderName();
            if (headerName.equals(""))
                continue;
            if (headerName.equals(header.getName()))
                buffer.closeHeader();
        }
        openedHeaders.remove(header);
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
        input.setName(myName);
        LispBuffer header = myEnvironment.findBuffer(myName);
        if (header == null)
            throw new NoBufferException(myName);

        LispBuffer currentBuffer = myEnvironment.getBufferCurrentForEditing();
        if (currentBuffer.getName().equals(myName))
            return;
        currentBuffer.getEditor().setHeaderComponent(input);

        /*for (LispBuffer buffer: myEnvironment.getBuffersWithNameNotBeginningWithSpace()) {
            if (buffer.getName().equals(myName))
                continue;
            ((IdeaEditor)buffer).setHeader(input);
        }  */

        setEditor(input.getEditor());
        myEnvironment.updateBuffer(this);

        EvaluateCommand command = new EvaluateCommand();
        command.registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, input);
        grabFocus();
    }

    @Override
    public void grabFocus () {
        if (!isHeaderBuffer())
            throw new RuntimeException("LispBuffer.grabFocus() wrong usage!");
        myEnvironment.switchToBuffer(myName);
        System.out.print("grab focus: ");
        myEnvironment.printBuffers();
        if (myEditor == null)
            throw new RuntimeException("null editor!");
        myEditor.getContentComponent().grabFocus();
    }

    private boolean isHeaderBuffer () {
        return (myName.equals(Environment.ourScratchBufferName) || this instanceof LispMiniBuffer);
    }

    @Override
    public void setBufferActive () {
        if (isHeaderBuffer()) {
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

    public void setHeader (IdeaEditor header) {
        myEditor.setHeaderComponent(header.getEditor().getContentComponent());
        myEnvironment.updateBuffer(this);
    }

    public void setHeader (JComponent header) {
        myEditor.setHeaderComponent(header);
        myEnvironment.updateBuffer(this);
    }

    public String getHeaderName () {
        try {
            return myEditor.getHeaderComponent().getName();
        } catch (NullPointerException e) {
            return "";
        }
    }

    public void closeHeader () {
        myEditor.setHeaderComponent(null);
        myEnvironment.updateBuffer(this);
    }

    public void close () {
        if (isHeaderBuffer()) {
            //setHeaderBufferActive();
            return;
        }
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myEditor.getProject());
        VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
        for (VirtualFile file: openedFiles) {
            if (file.getName().equals(myName)) {
                fileEditorManager.closeFile(file);
            }
        }
    }
}
