package org.jetbrains.emacs4ij.ide;

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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.BufferEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSyntaxTable;
import org.jetbrains.emacs4ij.jelisp.elisp.TextPropertiesHolder;
import org.jetbrains.emacs4ij.jelisp.exception.AssignNullDocument;
import org.jetbrains.emacs4ij.jelisp.exception.BufferOpenException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.NullBufferDocument;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.parser.BackwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;
import org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class IdeaBuffer extends TextPropertiesHolder implements LispBuffer {
  protected final String myName;
  protected final BufferEnvironment myEnvironment;
  protected List<LispMarker> myMarkers = new ArrayList<>();
  protected static Project ourProject;
  protected LispMarker myMark = new LispMarker();
  protected Document myDocument;

  private VirtualFile myVirtualFile;
  private LispKeymap myKeymap;
  private LispSyntaxTable mySyntaxTable;

  protected int myModificationsCount = 0;
  protected int mySaveModCount = 0;

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

  protected IdeaBuffer (String name, Environment environment, @Nullable Editor editor, @Nullable VirtualFile file) {
    myEnvironment = new BufferEnvironment(environment);
    myName = name;
    myVirtualFile = file;
    mySyntaxTable = SyntaxTable.getStandardSyntaxTable();
    //todo: set fundamental mode, it has StandardSyntaxTable set

    myEnvironment.defineBuffer(this);
    if (editor == null)
      return;
    openStandardBuffer(editor);
  }

  public IdeaBuffer(Environment environment, FileEditorManager fileEditorManager, VirtualFile file) {
    this(file.getName(), environment, null, file);
    setLocalVariable("default-directory", new LispString(file.getParent().getPath() + '/'));
    for (FileEditor fileEditor: fileEditorManager.getAllEditors(file)) {
      openStandardBuffer(((TextEditor) fileEditor).getEditor());
    }
    FileEditor selected = fileEditorManager.getSelectedEditor(file);
    if (selected != null)
      switchToWindow(((TextEditor) selected).getEditor(), true);
  }

  public IdeaBuffer(Environment environment, @NotNull VirtualFile file, @Nullable Editor editor) {
    this(file.getName(), environment, editor, file);
    setLocalVariable("default-directory", new LispString(file.getParent().getPath() + '/'));
  }

  public IdeaBuffer (Environment environment, String name, String defaultDir, LispToolWindow window) {
    this(name, environment, null, null);
    openToolBuffer(window);
    setLocalVariable("default-directory", new LispString(defaultDir));
  }

  private void openToolBuffer (LispToolWindow window) {
    myEnvironment.onToolBufferOpened(window);
    onOpen(((IdeaEditorWrapper) window.getEditor()).getEditor().getDocument());
  }

  protected final void openStandardBuffer(Editor editor) {
    myEnvironment.onBufferOpened(this, new IdeaEditorWrapper(editor));
    onOpen(editor.getDocument());
  }

  private void switchToWindow (final Editor editor, boolean switchBuffer) {
    myEnvironment.switchToWindow(new IdeaEditorWrapper(editor), switchBuffer);
  }

  @Nullable
  public VirtualFile getFile() {
    return myVirtualFile;
  }

  private void onOpen (Document document) {
    if (myDocument != null && myDocument != document)
      throw new BufferOpenException(myName);
    if (document == null)
      throw new AssignNullDocument(myName);
    if (myDocument != null)
      return;
    myDocument = document;
    myDocument.addDocumentListener(myDocumentListener);
  }

  public void reopen (Editor editor, VirtualFile file) {
    assert file == myVirtualFile;
    openStandardBuffer(editor);
  }

  @Override
  public LispObject getVariableValue(String name) {
    LispSymbol var = myEnvironment.find(name);
    if (var == null)
      throw new VoidVariableException(name);
    return var.getValue();
  }

  @Override
  public LispSymbol getVariable(String name) {
    return myEnvironment.find(name);
  }

  @Override
  public void defineVariable(LispSymbol variable) {
    for (LispSymbol symbol = variable, previous = variable; symbol != null; previous = symbol, symbol = symbol.next())  {
      myEnvironment.defineSymbol(new LispSymbol(symbol));
      List<LispSymbol> aliases = symbol.getAliases();
      if (aliases == null)
        continue;
      for (LispSymbol alias: aliases) {
        if (alias == previous)
          continue;
        defineVariableAliases(alias);
      }
    }
  }

  public void defineVariableAliases(LispSymbol variable) {
    myEnvironment.defineSymbol(new LispSymbol(variable));
    List<LispSymbol> aliases = variable.getAliases();
    if (aliases == null)
      return;
    for (LispSymbol alias: aliases)
      defineVariableAliases(alias);
  }

  @Override
  public boolean hasVariable(String variable) {
    return myEnvironment.containsSymbol(variable);
  }

  @Override
  public Map<LispSymbol, LispObject> getAllLocalVarValues() {
    Map<LispSymbol, LispObject> bufferLocalVariables = GlobalEnvironment.INSTANCE.getBufferLocalVariables();
    myEnvironment.getVariableEntries(bufferLocalVariables);
    return bufferLocalVariables;
  }

  @Override
  public void killVariable(String name) {
    myEnvironment.remove(name);
  }

  @Override
  public void reset() {
    myEnvironment.clear();
    //todo: set local keymap = nil; case table = standard-case-table; abbrev table = fundamental-mode-abbrev-table.value
    mySyntaxTable = SyntaxTable.getStandardSyntaxTable();
  }

  @Override
  public Environment getEnvironment() {
    return myEnvironment;
  }

  /**
   * Emacs determines whether the buffer is not a normal buffer for editing by enclosing it's name with stars: "*Help*".
   * In special cases a space is added at the beginning: " *Echo Area 0*".
   *
   * @return true if a buffer is a tool buffer
   */
  @Override
  public boolean isToolBuffer() {
    return myName.startsWith("*") || myName.startsWith(" ");
  }

  public static void setProject(Project project) {
    ourProject = project;
  }

  public static Project getProject() {
    return ourProject;
  }

  private void setLocalVariable (String name, LispObject value) {
    myEnvironment.setVariable(new LispSymbol(name, value));
  }

  @NotNull
  protected Document getDocument() {
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

  protected Editor getEditor() {
    return ((IdeaWindow)myEnvironment.getBufferLastSelectedWindow(this)).getEditor();
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
        ? new LispString(getDocument().getText().substring(start - 1, end - 1), getTextPropertiesInRange(start - 1, end - 1))
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
    try {
      return editor.logicalPositionToOffset(editor.getCaretModel().getLogicalPosition()) + 1;
    } catch (NullPointerException e) {
      System.out.print(1);
      throw e;
    }
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

  @Override
  public String getText() {
    return getDocument().getText();
  }

  @Override
  public void setText(String text) {
    getDocument().setText(text);
  }

  public void setPoint(int position) {
    try {
      getEditor().getCaretModel().moveToOffset(position - 1);
    } catch (IndexOutOfBoundsException e){
      System.out.print(1);
      throw e;
    }
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
        switchToWindow(fileEditorManager.openTextEditor(new OpenFileDescriptor(ourProject, file), true), false);
        return;
      }
    }
    //it is not in opened files, open it.
    assert myVirtualFile != null;
    Editor editor = ((TextEditor)fileEditorManager.openFile(myVirtualFile, true)[0]).getEditor();
    openStandardBuffer(editor);
    switchToWindow(editor, false);
  }

  @Override
  public void kill() {
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

  @Override
  public void setKeymap (LispKeymap keymap) {
    myKeymap = keymap;
  }

  @Override
  @Nullable
  public LispKeymap getKeymap() {
    return myKeymap;
  }

  @Override
  public void setModified (@Nullable LispObject flag) {
    if (Predicate.isNil(flag)) { //set unmodified, i.e. saved
      mySaveModCount = myModificationsCount;
    } else if (mySaveModCount >= myModificationsCount) { //synchronize last saved mod count and current mod count
      mySaveModCount = myModificationsCount++;
    }
    //todo: synchronize with virtual file state
  }

  @Override
  public void restoreModified(@Nullable LispObject flag) {
    mySaveModCount = Predicate.isNil(flag) ? myModificationsCount : 0;
    //todo: synchronize with virtual file state
  }

  @Override
  public boolean isModified() {
    return mySaveModCount < myModificationsCount;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof IdeaBuffer)) return false;

    IdeaBuffer that = (IdeaBuffer) o;

    if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;
    if (myVirtualFile != null ? !myVirtualFile.equals(that.myVirtualFile) : that.myVirtualFile != null)
      return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = super.hashCode();
    result = 31 * result + (myName != null ? myName.hashCode() : 0);
    result = 31 * result + (myVirtualFile != null ? myVirtualFile.hashCode() : 0);
    return result;
  }
}
