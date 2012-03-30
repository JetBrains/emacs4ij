package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.BackwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaBuffer implements LispBuffer {
    protected String myName;
    protected Editor myEditor;
    protected Environment myEnvironment;
    protected List<LispMarker> myMarkers = new ArrayList<>();
    private static Project ourProject;
    private LispMarker myMark = new LispMarker();
    protected boolean isChangedByMe = false;

    private HashMap<String, LispSymbol> myLocalVariables = new HashMap<String, LispSymbol>();

    protected IdeaBuffer() {}
    
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
                updateMarkersPositions(point(), shift, false);
                return;
            }
            if (shift > 0) { //insert
                updateMarkersPositions(myOldPosition, shift, false);
            }
        }
    };

    public IdeaBuffer(Environment environment, String name, String path, Editor editor) {
        myEnvironment = environment;
        myName = name;
        setEditor(editor);
        myEnvironment.defineBuffer(this);
        setLocalVariable("default-directory", new LispString(path));
    }
    
//    private void addLocalVar (String name, LispObject value) {
//        myLocalVariables.put(name, new LispSymbol(name, value, true));
//    }

    public static void setProject(Project project) {
        ourProject = project;
    }

    private void setLocalVariable (String name, LispObject value) {
        LispSymbol var = myLocalVariables.get(name);
        if (var == null)
            throw new VoidVariableException(name);
        var.setValue(value);
    }

    @Override
    public LispObject getLocalVariableValue (String name) {
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

//    @Override
//    public void defineLocalVariable(LispSymbol variable) {
//        myLocalVariables.put(variable.getName(), new LispSymbol(variable));
//    }

    @Override
    public void defineLocalVariable(LispSymbol variable, boolean noValue) {
        myLocalVariables.put(variable.getName(), new LispSymbol(variable, null));
    }

    @Override
    public LispObject evaluateLastForm() {
        String[] code = myEditor.getDocument().getText().split("\n");
        int line = myEditor.getCaretModel().getVisualPosition().getLine();
        int column = myEditor.getCaretModel().getVisualPosition().getColumn() - 1;
        if (code.length-1 < line) {
            line = code.length - 1;
            if (line < 0)
                throw new EndOfFileException();
            column = code[line].length() - 1;
        }
        if (code[line].length() - 1 < column) {
            column = code[line].length() - 1;
        }
        BackwardMultilineParser parser = new BackwardMultilineParser(code);
        LispObject parsed = parser.parse(line, column);
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
        if (myEditor != null) {
            myEditor.getDocument().removeDocumentListener(myDocumentListener);
        }
        myEditor = editor;
        if (myEditor != null) {
            myEditor.getDocument().addDocumentListener(myDocumentListener);
        }
    }

    public String toString() {
        return "#<buffer " + myName + ">";
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
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
    public void setPoint(int position) {
        myEditor.getCaretModel().moveToOffset(position);
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
    
    private void insertAt (final int position, final String insertion) {
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

    @Override
    public void setActive() {
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

    @Override
    public void kill () {
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
        VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
        for (VirtualFile file: openedFiles) {
            if (file.getName().equals(myName)) {
                fileEditorManager.closeFile(file);
            }
        }
    }

    //--------------- mark --------------------------------
    @Override
    public void addMarker (LispMarker marker) {
        if (!myMarkers.contains(marker) && myMark != marker)
            myMarkers.add(marker);
    }

    @Override
    public void removeMarker (LispMarker marker) {
        myMarkers.remove(marker);
    }

    @Override
    public boolean hasMarkersAt (int position) {
        for (LispMarker marker: myMarkers) {
            if (marker.isSet() && marker.getPosition() == position)
                return true;
        }
        return myMark.isSet() && myMark.getPosition() == position;
    }

    @Override
    public LispMarker getMark() {
        return myMark;
    }

    @Override
    public void setMark(LispMarker mark) {
        myMark = mark;
        if (myMarkers.contains(mark))
            myMarkers.remove(mark);
    }

    @Override
    public void insert(String insertion, int where) {
        if (StringUtil.isEmpty(insertion))
            return;
        insertAt(where - 1, insertion);
        updateMarkersPositions(where, insertion.length(), true);
        gotoChar(where + insertion.length());
    }
    
    private void updateMarkersPositions (int point, int shift, boolean moveAnyway) {
        for (LispMarker marker: myMarkers) {
            if (!marker.isSet())
                throw new Attention();
            marker.move(shift, point, moveAnyway);
        }
        if (myMark.isSet())
            myMark.move(shift, point, moveAnyway);
    }

    @Override
    public void insert(LispObject insertion, @Nullable LispMarker where) {
        String ins = insertion.toString();
        if (insertion instanceof LispString) {
            LispObject kbd = evaluateString(myEnvironment, "(kbd " + insertion.toString() + ")");
            ins = kbd instanceof LispString ? ((LispString) kbd).getData() : kbd.toString();
        }
        if (where != null && !where.isSet())
            throw new MarkerPointsNowhereException();
        int pos = where == null ? point() : where.getPosition();
        insert(ins, pos);
    }

    @Override
    public void insert(LispObject insertion) {
        insert(insertion, null);
    }

    @Override
    public void insert(String insertion) {
        insert(insertion, point());
    }

    private LispObject evaluateString (Environment environment, String code) {
        return new ForwardParser().parseLine(code).evaluate(environment);
    }
}
