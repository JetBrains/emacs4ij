package org.jetbrains.emacs4ij.jelisp.elisp.text;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public abstract class TextPropertiesHolder {
  protected List<TextPropertiesInterval> myIntervals = new ArrayList<>();

  public boolean noTextProperties() {
    return myIntervals.isEmpty();
  }

  protected abstract int size();

  protected int getCharacterShift() {
    return 0;
  }

  public final void setTextProperties (List<TextPropertiesInterval> intervals) {
    myIntervals = intervals;
  }

  public final List<TextPropertiesInterval> getIntervals(){
    return myIntervals;
  }

  /**
   * @param givenStart = left bound of interval
   * @param givenEnd = right bound of interval
   * @param propertyList to work with
   * @return true if any properties were added or changed, false otherwise
   */
  public final boolean actOnTextProperties(int givenStart, int givenEnd, LispList propertyList, Action action) {
    Range range;
    try {
      int start = givenStart + getCharacterShift();
      int end = givenEnd + getCharacterShift();
      range = new Range(start, end, 0, size());
    } catch (ArgumentOutOfRange e) {
      throw new ArgumentOutOfRange(givenStart, givenEnd);
    }
    if (range.isEmpty())
      return false;
    if (noTextProperties()) {
      switch (action) {
        case ADD:
          myIntervals.add(new TextPropertiesInterval(range.getStart(), range.getEnd(), 0, size(), propertyList));
          return true;
        case SET:
          if (propertyList.equals(LispList.list(LispSymbol.NIL, LispSymbol.NIL)))
            return false;
          myIntervals.add(new TextPropertiesInterval(range.getStart(), range.getEnd(), 0, size(), propertyList));
          return true;
        case REMOVE:case REMOVE_LIST:
          return false;
      }
    }
    List<TextPropertiesInterval> additionalIntervals = new ArrayList<>();
    boolean changed = false;
    for (Iterator<TextPropertiesInterval> iterator = myIntervals.iterator(); iterator.hasNext(); ) {
      TextPropertiesInterval interval = iterator.next();
      if (!interval.getRange().contains(range.getStart())) {
        continue;
      }
      int oldEnd = interval.getRange().getEnd();
      changed = interval.extractIntervalAndPerformAction(range, propertyList, action, additionalIntervals);
      if (interval.hasNoProperties())
        iterator.remove();
      if (oldEnd < range.getEnd()) {
        range.setStart(oldEnd);
        continue;
      }
      break;
    }
    myIntervals.addAll(additionalIntervals);
    Collections.sort(myIntervals);
    return changed;
  }

  public final String intervalsString() {
    StringBuilder sb = new StringBuilder();
    for (TextPropertiesInterval interval: myIntervals) {
      sb.append(interval.toString()).append(" ");
    }
    return sb.toString().trim();
  }

  public List<TextPropertiesInterval> getTextPropertiesInRange(int start, int end) {
    if (start < 0 || end > size())
      throw new ArgumentOutOfRange(start, end);
    List<TextPropertiesInterval> list = new ArrayList<>();
    for (TextPropertiesInterval interval : myIntervals) {
      if (interval.getRange().getEnd() <= start) {
        continue;
      }
      if (interval.getRange().getStart() >= end) {
        break;
      }
      list.add(interval.extract(start, end));
    }
    Collections.sort(list);
    return list;
  }

  @Nullable
  private TextPropertiesInterval getTextPropertiesIntervalFor (int position) {
    if (position == size())
      return null;
    List<TextPropertiesInterval> properties = getTextPropertiesInRange(position, position);
    assert properties.size() <= 1 : "Multiple text properties intervals for position";
    if (properties.isEmpty())
      return null;
    return properties.get(0);
  }

  public final LispObject getTextPropertiesAt (int position) {
    TextPropertiesInterval interval = getTextPropertiesIntervalFor(position);
    return interval == null
        ? LispSymbol.NIL
        : interval.getPropertyList();
  }

  public final LispObject getTextPropertyAt (int position, LispObject property) {
    TextPropertiesInterval interval = getTextPropertiesIntervalFor(position);
    return interval == null
        ? LispSymbol.NIL
        : Core.thisOrNil(interval.getProperties().get(property));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof TextPropertiesHolder)) return false;

    TextPropertiesHolder that = (TextPropertiesHolder) o;

    if (myIntervals != null ? !myIntervals.equals(that.myIntervals) : that.myIntervals != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    return myIntervals != null ? myIntervals.hashCode() : 0;
  }




}
