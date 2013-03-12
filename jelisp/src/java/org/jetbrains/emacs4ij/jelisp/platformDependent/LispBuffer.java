package org.jetbrains.emacs4ij.jelisp.platformDependent;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.BufferEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.HasTextPropertiesHolder;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSyntaxTable;
import org.jetbrains.emacs4ij.jelisp.elisp.TextPropertiesHolder;
import org.jetbrains.emacs4ij.jelisp.exception.Attention;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.parser.BackwardMultilineParser;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;
import org.jetbrains.emacs4ij.jelisp.subroutine.SyntaxTable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public abstract class LispBuffer implements LispObject, HasTextPropertiesHolder {
  private final String myName;
  private final BufferEnvironment myEnvironment;

  private LispMarker myMark = new LispMarker();
  private List<LispMarker> myMarkers = new ArrayList<>();
  private LispKeymap myKeymap;
  private LispSyntaxTable mySyntaxTable;

  private int myModificationsCount = 0;
  private int mySaveModCount = 0;

  private TextPropertiesHolder myPropertiesHolder = new TextPropertiesHolder() {
    @Override
    protected int size() {
      return LispBuffer.this.size();
    }
    @Override
    protected int getCharacterShift() {
      return -1;
    }
  };

  protected LispBuffer(String name, Environment environment) {
    myEnvironment = new BufferEnvironment(environment);
    myName = name;
    mySyntaxTable = SyntaxTable.getStandardSyntaxTable();
    //todo: set fundamental mode, it has StandardSyntaxTable set

    myEnvironment.defineBuffer(this);
  }

  @Override
  public TextPropertiesHolder getTextPropertiesHolder() {
    return myPropertiesHolder;
  }

  public abstract int size();
  public abstract void kill();
  public abstract void closeHeader();

  public void reset() {
    myEnvironment.clear();
    //todo: set local keymap = nil; case table = standard-case-table; abbrev table = fundamental-mode-abbrev-table.value
    mySyntaxTable = SyntaxTable.getStandardSyntaxTable();
  }

  /**
   * Emacs determines whether the buffer is not a normal buffer for editing by enclosing it's name with stars: "*Help*".
   * In special cases a space is added at the beginning: " *Echo Area 0*".
   *
   * @return true if a buffer is a tool buffer
   */
  public boolean isToolBuffer() {
    return myName.startsWith("*") || myName.startsWith(" ");
  }

  public void setActive() {
    myEnvironment.setBufferCurrentForEditing(this);
  }

  //-------------------------------------------------------------------------------
  //     text
  //-------------------------------------------------------------------------------
  public abstract String getText();
  public abstract void setText (final String text);
  protected abstract void insertAt (final int position, final String insertion);
  public abstract void replace(final int from, final int to, final String text);

  public LispString substring(int start, int end, boolean withProperties) {
    return withProperties
        ? new LispString(getText().substring(start - 1, end - 1), myPropertiesHolder.getTextPropertiesInRange(start - 1, end - 1))
        : new LispString(getText().substring(start - 1, end - 1));
  }

  public void insert(String insertion, int where) {
    if (StringUtil.isEmpty(insertion))
      return;
    insertAt(where - 1, insertion);
    gotoChar(where + insertion.length());
  }

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

  public void insert(LispObject insertion) {
    insert(insertion, null);
  }

  public void insert(String insertion) {
    insert(insertion, point());
  }

  private LispObject kbd (Environment environment, LispString keys) {
    return LispList.list(new LispSymbol("kbd"), keys).evaluate(environment);
  }

  //-------------------------------------------------------------------------------
  //     buffer variables
  //-------------------------------------------------------------------------------

  public LispObject getVariableValue(String name) {
    LispSymbol var = myEnvironment.find(name);
    if (var == null)
      throw new VoidVariableException(name);
    return var.getValue();
  }

  public LispSymbol getVariable(String name) {
    return myEnvironment.find(name);
  }

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

  public boolean hasVariable(String variable) {
    return myEnvironment.containsSymbol(variable);
  }

  public Map<LispSymbol, LispObject> getAllLocalVarValues() {
    Map<LispSymbol, LispObject> bufferLocalVariables = GlobalEnvironment.INSTANCE.getBufferLocalVariables();
    myEnvironment.getVariableEntries(bufferLocalVariables);
    return bufferLocalVariables;
  }

  protected final void setLocalVariable (String name, LispObject value) {
    myEnvironment.setVariable(new LispSymbol(name, value));
  }

  public void killVariable(String name) {
    myEnvironment.remove(name);
  }

  //-------------------------------------------------------------------------------
  //     modification
  //-------------------------------------------------------------------------------

  public void setModified (@Nullable LispObject flag) {
    if (Predicate.isNil(flag)) { //set unmodified, i.e. saved
      mySaveModCount = myModificationsCount;
    } else if (mySaveModCount >= myModificationsCount) {
      //synchronize last saved mod count and current mod count
      mySaveModCount = myModificationsCount++;
    }
    //todo: synchronize with virtual file state
  }

  public void restoreModified(@Nullable LispObject flag) {
    mySaveModCount = Predicate.isNil(flag) ? myModificationsCount : 0;
    //todo: synchronize with virtual file state
  }

  public boolean isModified() {
    return mySaveModCount < myModificationsCount;
  }

  //-------------------------------------------------------------------------------
  //     navigation
  //-------------------------------------------------------------------------------
  public abstract int point();
  public abstract void setPoint(int position);

  public int pointMin() {
    return 1;
  }

  public int pointMax() {
    return size() + 1;
  }

  public int precedingCharacter() {
    if (point() == pointMin())
      return 0;
    return (int)getText().charAt(point()-2);
  }

  public int followingCharacter() {
    if (point() == pointMax())
      return 0;
    return (int)getText().charAt(point()-1);
  }

  public String forwardChar(int shift) {
    return gotoChar(point() + shift);
  }

  public String gotoChar(int position) {
    String message = "";
    if (position < pointMin()) {
      position = pointMin();
      message = "Beginning of buffer";
    }
    else if (position > pointMax()) {
      position = pointMax();
      message = "End of buffer";
    }
    setPoint(position);
    return message;
  }


  //-------------------------------------------------------------------------------
  //     markers
  //-------------------------------------------------------------------------------

  public void addMarker (LispMarker marker) {
    if (!myMarkers.contains(marker) && myMark != marker)
      myMarkers.add(marker);
  }

  public void removeMarker (LispMarker marker) {
    myMarkers.remove(marker);
  }

  public boolean hasMarkersAt (int position) {
    for (LispMarker marker: myMarkers) {
      if (marker.isSet() && marker.getPosition() == position)
        return true;
    }
    return myMark.isSet() && myMark.getPosition() == position;
  }

  public LispMarker getMark() {
    return myMark;
  }

  public void setMark(LispMarker mark) {
    myMark = mark;
    if (myMarkers.contains(mark))
      myMarkers.remove(mark);
  }

  protected final void updateMarkersPositions (int point, int shift, boolean moveAnyway) {
    for (LispMarker marker: myMarkers) {
      if (!marker.isSet())
        throw new Attention();
      marker.move(shift, point, moveAnyway);
    }
    if (myMark.isSet())
      myMark.move(shift, point, moveAnyway);
  }

  //-------------------------------------------------------------------------------
  //     getters & setters; basic
  //-------------------------------------------------------------------------------
  protected abstract int getLine();
  protected abstract int getColumn();

  public LispObject evaluateLastForm() {
    String[] code = getText().split("\n");
    int line = getLine();
    int column = getColumn();
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

  public String getName() {
    return myName;
  }

  public BufferEnvironment getEnvironment() {
    return myEnvironment;
  }

  public LispSyntaxTable getSyntaxTable() {
    return mySyntaxTable;
  }

  public void setSyntaxTable(LispSyntaxTable table) {
    mySyntaxTable = table;
  }

  public void setKeymap (LispKeymap keymap) {
    myKeymap = keymap;
  }

  @Nullable
  public LispKeymap getKeymap() {
    return myKeymap;
  }

  @Override
  public LispObject evaluate(Environment environment) {
    return this;
  }

  @Override
  public String toString() {
    return "#<buffer " + myName + ">";
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof LispBuffer)) return false;

    LispBuffer that = (LispBuffer) o;

    if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    return myName != null ? myName.hashCode() : 0;
  }
}
