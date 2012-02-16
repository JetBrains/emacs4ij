package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.impl.EditorImpl;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.emacs4ij.jelisp.BackwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaBuffer extends LispObject implements LispBuffer {
    protected String myName;
    protected Editor myEditor;
    protected Environment myEnvironment;
    protected ArrayList<LispMarker> myMarkers = new ArrayList<>();
    private static Project ourProject;
    private boolean isAlive = true;

    private HashMap<String, LispSymbol> myLocalVariables = new HashMap<String, LispSymbol>();

    protected IdeaBuffer() {}

    public IdeaBuffer(Environment environment, String name, String path, Editor editor) {
        myEnvironment = environment;
        myName = name;
        myEditor = editor;
        myEnvironment.defineBuffer(this);
        setLocalVariable("default-directory", new LispString(path));
        //setLocalVariable("mark-active", LispSymbol.ourNil); == default value


        //addLocalVar("is-alive", LispSymbol.ourT);
       // addLocalVar("my-mark", new LispMarker());

       // addLocalVar("mark-ring", LispList.list());
    }
    
    private void addLocalVar (String name, LObject value) {
        myLocalVariables.put(name, new LispSymbol(name, value, true));
    }

    public static void setProject(Project project) {
        ourProject = project;
    }

    private void setLocalVariable (String name, LObject value) {
        LispSymbol var = myLocalVariables.get(name);
        if (var == null)
            throw new VoidVariableException(name);
        var.setValue(value);
    }

    @Override
    public LObject getLocalVariableValue (String name) {
        LispSymbol localVar = getLocalVariable(name);
        return localVar.getValue();
    }

    @Override
    public LispSymbol getLocalVariable(String name) {
        LispSymbol var = myLocalVariables.get(name);
        if (var == null)
            throw new VoidVariableException(name);
        return var;
    }

    @Override
    public void defineLocalVariable(LispSymbol variable) {
        myLocalVariables.put(variable.getName(), new LispSymbol(variable));
    }

    @Override
    public void defineLocalVariable(LispSymbol variable, boolean noValue) {
        myLocalVariables.put(variable.getName(), new LispSymbol(variable, null));
    }

    @Override
    public LObject evaluateLastForm() {
        String[] code = myEditor.getDocument().getText().split("\n");
        int line = myEditor.getCaretModel().getVisualPosition().getLine();
        int column = myEditor.getCaretModel().getVisualPosition().getColumn() - 1;
        if (code.length-1 < line) {
            line = code.length - 1;
            if (line < 0)
                throw new LispException("End of file during parsing");
            column = code[line].length() - 1;
        }
        BackwardMultilineParser parser = new BackwardMultilineParser(code);
        LObject parsed = parser.parse(line, column);
        return parsed.evaluate(myEnvironment);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        IdeaBuffer that = (IdeaBuffer) o;

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
        throw new RuntimeException("Cannot evaluate buffer!");
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

    protected void setHeaderBufferActive () {
        if (myEnvironment.getServiceBuffer(myName) == null)
            throw new NoBufferException(myName);
        if (myEditor == null)
            throw new RuntimeException("null editor!");
        if (!(this instanceof IdeaMiniBuffer))
            write("");
        //myEnvironment.updateServiceBuffer(this);
        myEditor.getContentComponent().grabFocus();
    }

    private boolean isHeaderBuffer () {
        return this instanceof LispMiniBuffer;
        //return (myName.equals(GlobalEnvironment.ourScratchBufferName) || this instanceof LispMiniBuffer);
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
    public void closeHeader () {
        if (myEditor.getHeaderComponent() == null)
            return;
        myEditor.setHeaderComponent(null);
    }

    public void kill () {
        if (isHeaderBuffer()) {
            LispBuffer displayedBuffer = myEnvironment.findBufferSafe(getDisplayedBufferName());
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

    @Override
    public void showMessage(String message) {
        Messages.showInfoMessage(message, "Elisp message");
    }

    //--------------- mark --------------------------------
    @Override
    public void addMarker (LispMarker marker) {
        if (!myMarkers.contains(marker))
            myMarkers.add(marker);
    }

    @Override
    public void removeMarker (LispMarker marker) {
        myMarkers.remove(marker);
    }

    @Override
    public boolean hasMarkersAt (int position) {
        for (LispMarker marker: myMarkers) {
            if (marker.getPosition() instanceof LispInteger && ((LispInteger) marker.getPosition()).getData() == position)
                return true;
        }
        return false;
    }

   /* @Override
    public Integer getMark() {
        return myMark;
    }   */

   /* @Override
    public void setMark(int position) {
        myMark = position;
        isMarkActive = true;
    }

    @Override
    public void pushMark(@Nullable Integer position, boolean activate) {
        if (position == null) {
            position = point();
        }
        if (myMark != null) {
            LObject markRingMax = GlobalEnvironment.getInstance().find("mark-ring-max").getValue();
            if (!(markRingMax instanceof LispInteger || markRingMax instanceof LispMarker))
                throw new WrongTypeArgumentException("mark-ring-max", markRingMax.toString());
            int max = (markRingMax instanceof LispInteger) ? ((LispInteger) markRingMax).getData() : ((LispMarker)markRingMax).getPosition();
            while (myMarkRing.size() >= max)
                myMarkRing.removeLast();
            myMarkRing.push(myMark);
        }
        myMark = position;
        isMarkActive = activate;
    }

    @Override
    public void popMark() {
        if (myMarkRing.isEmpty())
            return;
        isMarkActive = false;
        myMark = myMarkRing.pop();
    }  */
}
