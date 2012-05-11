package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import org.jetbrains.emacs4ij.jelisp.parser.BackwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaBuffer extends TextPropertiesHolder implements LispBuffer {
    protected final String myName;
    protected final Environment myEnvironment;
    protected List<LispMarker> myMarkers = new ArrayList<>();
    protected static Project ourProject;
    protected LispMarker myMark = new LispMarker();
    protected Map<String, LispSymbol> myLocalVariables = new HashMap<>();
    protected Document myDocument;
    private VirtualFile myVirtualFile = null;

    private LispSyntaxTable mySyntaxTable;

    protected final DocumentListener myDocumentListener = new DocumentListener() {
        private int myOldPosition;
        @Override
        public void beforeDocumentChange(DocumentEvent documentEvent) {
            myOldPosition = point();
        }

        @Override
        public void documentChanged(DocumentEvent documentEvent) {
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

    protected IdeaBuffer (Environment environment, String name, @Nullable Editor editor) {
        myEnvironment = environment;
        myName = name;
        mySyntaxTable = SyntaxTable.getStandardSyntaxTable();
        //todo: set fundamental mode, it has StandardSyntaxTable set

        myEnvironment.defineBuffer(this);
        if (editor == null)
            return;
        myEnvironment.onWindowOpened(this, editor);
    }

    public IdeaBuffer(Environment environment, FileEditorManager fileEditorManager, VirtualFile file) {
        this(environment, file.getName(), null);
        myVirtualFile = file;
        setLocalVariable("default-directory", new LispString(file.getParent().getPath() + '/'));
        for (FileEditor fileEditor: fileEditorManager.getAllEditors(file)) {
            myEnvironment.onWindowOpened(this, ((TextEditor)fileEditor).getEditor());
        }
        FileEditor selected = fileEditorManager.getSelectedEditor(file);
        if (selected != null)
            myEnvironment.switchToWindow(((TextEditor)selected).getEditor(), true);
    }

    public IdeaBuffer(Environment environment, VirtualFile file, @Nullable Editor editor) {
        this(environment, file.getName(), editor);
        myVirtualFile = file;
        setLocalVariable("default-directory", new LispString(file.getParent().getPath() + '/'));
    }

    public VirtualFile getFile() {
        return myVirtualFile;
    }

    @Override
    public void onOpen (Document document) {
        if (myDocument != null && myDocument != document)
            throw new BufferOpenException(myName);
        if (document == null)
            throw new AssignNullDocument(myName);
        if (myDocument != null)
            return;
        myDocument = document;
        myDocument.addDocumentListener(myDocumentListener);
    }

    @Override
    public void reopen (Editor editor, VirtualFile file) {
        assert file == myVirtualFile;
        myEnvironment.onWindowOpened(this, editor);
    }

    public static void setProject(Project project) {
        ourProject = project;
    }

    public static Project getProject() {
        return ourProject;
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

    @Override
    public void defineLocalVariable(LispSymbol variable, boolean noValue) {
        myLocalVariables.put(variable.getName(), new LispSymbol(variable, null));
    }

    @Override
    public Document getDocument() {
        if (myDocument == null)
            throw new NullBufferDocument(myName);
        return myDocument;
    }

    @Override
    public LispObject evaluateLastForm() {
        String[] code = getDocument().getText().split("\n");
        int line = getEditor().getCaretModel().getVisualPosition().getLine();
        int column = getEditor().getCaretModel().getVisualPosition().getColumn() - 1;
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
    public Editor getEditor() {
        return myEnvironment.getBufferLastSelectedWindow(this).getEditor();
    }

    @Override
    public void setSyntaxTable(LispSyntaxTable table) {
        mySyntaxTable = table;
    }

    @Override
    public LispSyntaxTable getSyntaxTable() {
        return mySyntaxTable;
    }

    @Override
    public LispString substring(int start, int end, boolean withProperties) {
        return withProperties
                ? new LispString(getDocument().getText().substring(start - 1, end - 1), getTextPropertiesInRange(start, end))
                : new LispString(getDocument().getText().substring(start - 1, end - 1));
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
    public int size() {
        return getDocument().getTextLength();
    }

    @Override
    public int point() {
        Editor editor = getEditor();
        return editor.logicalPositionToOffset(editor.getCaretModel().getLogicalPosition()) + 1;
    }

    @Override
    public int followingCharacter() {
        if (point() == pointMax())
            return 0;
        return (int)getDocument().getText().charAt(point()-1);
    }

    @Override
    public int precedingCharacter() {
        if (point() == pointMin())
            return 0;
        return (int)getDocument().getText().charAt(point()-2);
    }

    public void setPoint(int position) {
        getEditor().getCaretModel().moveToOffset(position);
    }

    public int pointMin() {
        return 1;
    }

    public int pointMax() {
        return size()+1;
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
        getEditor().getCaretModel().moveToOffset(position-1);
        return message;
    }

    protected void write (final String text) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        getDocument().setText(text);
                        gotoChar(pointMax());
                    }
                });
            }
        });
    }

    private void insertAt (final int position, final String insertion) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        getDocument().insertString(position, insertion);
                    }
                });
            }
        });
    }

    @Override
    public void replace(final int from, final int to, final String text) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        getDocument().replaceString(from - 1, to - 1, text);
                        gotoChar(from + text.length());
                    }
                });
            }
        });
    }


    @Override
    public String forwardChar (int shift) {
        return gotoChar(point() + shift);
    }

    @Override
    public void setActive() {
        myEnvironment.setBufferCurrentForEditing(this);

        FileEditorManager fileEditorManager = FileEditorManager.getInstance(ourProject);
        VirtualFile[] openedFiles = fileEditorManager.getOpenFiles();
        for (VirtualFile file: openedFiles) {
            if (file.getName().equals(myName)) {
                myEnvironment.switchToWindow(
                        fileEditorManager.openTextEditor(new OpenFileDescriptor(ourProject, file), true),
                        false);
                return;
            }
        }
        //it is not in opened files, open it.
        assert myVirtualFile != null;
        Editor editor = ((TextEditor)fileEditorManager.openFile(myVirtualFile, true)[0]).getEditor();
        myEnvironment.onWindowOpened(this, editor);
        myEnvironment.switchToWindow(editor, false);
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
        getDocument().removeDocumentListener(myDocumentListener);
        myDocument = null;
    }

    @Override
    public void closeHeader () {
        try {
            Editor editor = getEditor();
            if (editor == null)
                return;
            if (editor.getHeaderComponent() == null)
                return;
            editor.setHeaderComponent(null);
        } catch (LispException e) {
            //the buffer was killed, skip
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
            LispObject kbd = kbd(myEnvironment, (LispString) insertion);
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

    private LispObject kbd (Environment environment, LispString keys) {
        return LispList.list(new LispSymbol("kbd"), keys).evaluate(environment);
    }
}
