package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.BufferEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.MarkerOrInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.elisp.StringRegexpUtil;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidBackslashInReplacementException;
import org.jetbrains.emacs4ij.jelisp.exception.NoMatchData;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;

public abstract class Match {
  private Match() {}

  private static List<Integer> myLastMatch = new ArrayList<>();
  private static LispBuffer myBuffer = null;

  public static void clearData() {
    myLastMatch.clear();
    myBuffer = null;
  }

  //fro test
  public static LispBuffer getBuffer() {
    return myBuffer;
  }

  //for test
  public static List<Integer> getLastMatch() {
    return Collections.unmodifiableList(myLastMatch);
  }

  private static void addMatch (Matcher matcher, int group, List<Integer> insertions, int max) {
    if (insertions.isEmpty()) {
      myLastMatch.add(matcher.start(group));
      myLastMatch.add(matcher.end(group));
      return;
    }
    int nInsertionsBefore = 0;
    //add start
    int value = matcher.start(group);
    for (; nInsertionsBefore < insertions.size() && insertions.get(nInsertionsBefore) < value; nInsertionsBefore++) {}
    int i = value - nInsertionsBefore;
    myLastMatch.add(i < 0 ? 0 : (i >= max ? max : i));

    //add end
    value = matcher.end(group);
    for (; nInsertionsBefore < insertions.size() && insertions.get(nInsertionsBefore) <= value; nInsertionsBefore++) {}
    i = value - nInsertionsBefore;
    myLastMatch.add(i < 0 ? 0 : (i >= max ? max : i));
  }

  public static void registerSearchResult (@NotNull Matcher matcher, List<Integer> insertions, int max) {
    clearData();
    if (!insertions.isEmpty()) {
      Collections.sort(insertions);
    }
    try {
      for (int i = 0; i <= matcher.groupCount(); i++) {
        addMatch(matcher, i, insertions, max);
      }
    } catch (IllegalStateException e) {
      myLastMatch.clear();
    }
  }

  private static LispObject getMatch (LispInteger subExp, boolean start) {
    if (myLastMatch.isEmpty())
      throw new NoMatchData();
    return getMatch(subExp.getData(), start);
  }

  private static LispObject getMatch (int index, boolean start) {
    if (index < 0) throw new ArgumentOutOfRange(index, 0);
    try {
      Integer value = myLastMatch.get(index * 2 + (start ? 0 : 1));
      if (value == null) return LispSymbol.NIL;
      return myBuffer == null ? new LispInteger(value) : new LispMarker(value, myBuffer);
    } catch (IndexOutOfBoundsException e) {
      return LispSymbol.NIL;
    }
  }

  @Subroutine("match-beginning")
  public static LispObject matchBeginning (LispInteger subExp) {
    return getMatch(subExp, true);
  }

  @Subroutine("match-end")
  public static LispObject matchEnd (LispInteger subExp) {
    return getMatch(subExp, false);
  }

  /**
   * if (reset && (list contains any marker)) then reset all that markers to nowhere
   */
  private static void resetMarkersInListIfNeeded (LispObject reset, LispList list) {
    if (Predicate.isNil(reset))
      return;
    for (LispObject item: list.toLispObjectList()) {
      if (item instanceof LispMarker) {
        ((LispMarker) item).set(null, null);
      }
    }
  }

  @Subroutine("match-data")
  public static LispList matchData(@Optional LispObject integers, LispObject reuse, LispObject reset) {
    List<LispObject> result = new ArrayList<>();
    if (!Predicate.isNil(integers)) {
      for (Integer item: myLastMatch) {
        result.add(item == null ? LispSymbol.NIL : new LispInteger(item));
      }
      if (myBuffer != null)
        result.add(myBuffer);
    } else if (myBuffer != null) {
      for (Integer item: myLastMatch) {
        result.add(item == null ? LispSymbol.NIL : new LispMarker(item, myBuffer));
      }
    } else {
      for (Integer item: myLastMatch) {
        result.add(item == null ? LispSymbol.NIL : new LispInteger(item));
      }
    }

    LispList reuseList = reuse instanceof LispList ? (LispList) reuse : null;
    if (reuseList == null)
      return LispList.list(result);

    resetMarkersInListIfNeeded(reset, reuseList);

    ((LispList) reuse).resetWith(LispList.list(result));
    return (LispList) reuse;
  }

  private static int getPosition (MarkerOrInteger item) {
    Integer position = item.getPosition();
    return position == null
        ? myBuffer == null ? 0 : 1
        : position;
  }

  @Subroutine("set-match-data")
  public static LispSymbol setMatchData (LispObject list, @Optional LispObject reset) {
    if (list.equals(LispSymbol.NIL) || (list instanceof LispList && ((LispList) list).isEmpty())) {
     clearData();
    } else if (list instanceof LispList) {
      List<LispObject> objects = ((LispList) list).toLispObjectList();
      LispBuffer buffer = null;
      for (int j = 0, objectsSize = objects.size(); j < objectsSize; j++) {
        LispObject item = objects.get(j);
        if (!Predicate.isIntegerOrMarker(item)) throw new WrongTypeArgumentException("interger-or-marker-p", item);
        if (item instanceof LispMarker && ((LispMarker) item).getBuffer() != null && j % 2 == 0 && j + 1 < objectsSize) {
          buffer = ((LispMarker) item).getBuffer();
        }
      }
      myLastMatch.clear();
      myBuffer = buffer;
      for (int i = 0; i + 1 < objects.size(); i += 2) {
        myLastMatch.add(getPosition((MarkerOrInteger) objects.get(i)));
        myLastMatch.add(getPosition((MarkerOrInteger) objects.get(i + 1)));
      }
      resetMarkersInListIfNeeded(reset, (LispList) list);
    } else {
      throw new WrongTypeArgumentException("listp", list);
    }
    return LispSymbol.NIL;
  }

  private static int getMatchIndex (LispInteger index, boolean start) {
    LispObject match = getMatch(index, start);
    if (match instanceof MarkerOrInteger) {
      Integer position = ((MarkerOrInteger) match).getPosition();
      return position == null ? -1 : position;
    }
    return -1;
  }

  private static int getMatchIndex (int index, boolean start) {
    LispObject match = getMatch(index, start);
    if (match instanceof MarkerOrInteger) {
      Integer position = ((MarkerOrInteger) match).getPosition();
      return position == null ? -1 : position;
    }
    return -1;
  }

  @Subroutine("replace-match")
  public static LispObject replaceMatch (Environment environment, LispString newText,
                                         @Optional LispObject fixedCase, LispObject literal,
                                         LispObject string, LispObject subExp) {
    if (!Predicate.isNil(subExp) && !(subExp instanceof LispInteger))
      throw new WrongTypeArgumentException("integerp", subExp);
    LispInteger index = Predicate.isNil(subExp) ? new LispInteger(0) : (LispInteger)subExp;
    int from = getMatchIndex(index, true);
    int to = getMatchIndex(index, false);
    try {
      String text = applyFixedCase(environment, fixedCase, newText, index, string);
      String source = string instanceof LispString
          ? ((LispString)string).getData()
          : environment.getBufferCurrentForEditing().getText();

      text = applyLiteral(literal, text, index, source, string instanceof LispString ? 0 : -1);

      if (Predicate.isNil(string)) {
        environment.getBufferCurrentForEditing().replace(from, to, text);
        return LispSymbol.NIL;
      }
      if (!(string instanceof LispString))
        throw new WrongTypeArgumentException("stringp", string);
      return ((LispString)string).replace(from, to, text);

    } catch (NoMatchData e) {
      Core.error(JelispBundle.message("replace.no.match"));
      return LispSymbol.NIL;
    } catch (ArgumentOutOfRange e2) {
      throw new ArgumentOutOfRange (from, to);
    }
  }

  private static String applyFixedCase (Environment environment, LispObject fixedCase, final LispString newText,
                                        LispInteger index, LispObject string) {
    if (!Predicate.isNil(fixedCase))
      return newText.getData();

    LispObject toReplaceStr = Core.functionCall(environment, new LispSymbol("match-string"), index, Core.thisOrNil(string));
    if (toReplaceStr.equals(LispSymbol.NIL))
      return newText.getData();

    String toReplace = ((LispString)toReplaceStr).getData();

    boolean hasLowercase = false;
    boolean hasUppercase = false;
    boolean hasNonCapitalizedWord = false;
    for (int i = 0; i < toReplace.length(); i++) {
      char c = toReplace.charAt(i);
      if (Character.isLowerCase(c)) {
        hasLowercase = true;
        if (!hasNonCapitalizedWord && (i == 0 || (i > 0 && !SyntaxTable.isWord(environment, toReplace.charAt(i - 1)))))
          hasNonCapitalizedWord = true;
        continue;
      }
      if (Character.isUpperCase(c)) {
        hasUppercase = true;
      }
    }
    if (!hasLowercase && hasUppercase)
      return newText.getData().toUpperCase();
    if (!hasNonCapitalizedWord && hasLowercase)
      return newText.capitalize(environment);
    return newText.getData();
  }

  private static String applyLiteral (LispObject literal, String replacement, LispInteger index, String source, int add) {
    if (!Predicate.isNil(literal))
      return replacement;

    StringBuilder realReplacement = new StringBuilder();
    int previousSubgroupIndex = 0;
    for (int i = 0; i < replacement.length(); i++) {
      char c = replacement.charAt(i);
      if (c == '\\') {
        if (i + 1 >= replacement.length())
          throw new InvalidBackslashInReplacementException();

        char next = replacement.charAt(i + 1);

        if (next == '\\') {
          realReplacement.append(replacement.substring(previousSubgroupIndex, i));
          i++;
          previousSubgroupIndex = i;
          continue;
        }
        String head = "";
        String tail = "";
        if (i >= previousSubgroupIndex)
          head = replacement.substring(previousSubgroupIndex, i);

        if (next == '&') {
          tail = source.substring(getMatchIndex(index, true), getMatchIndex(index, false));
        } else if (next >= '1' && next <= '9') {
          if (next <= myLastMatch.size() / 2 + '0' && getMatchIndex(next - '0', true) >= 0) {
            tail = source.substring(getMatchIndex(next - '0', true) + add, getMatchIndex(next - '0', false) + add);
          }
        } else
          throw new InvalidBackslashInReplacementException();

        realReplacement.append(head).append(tail);
        previousSubgroupIndex = i + 2;
        i++;
      }
    }
    if (previousSubgroupIndex < replacement.length())
      realReplacement.append(replacement.substring(previousSubgroupIndex));

    return realReplacement.toString();
  }

  @Subroutine("looking-at")
  public static LispSymbol lookingAt(Environment environment, LispString regexp) {
    LispBuffer current = environment.getBufferCurrentForEditing();
    BufferEnvironment currentEnv = current.getEnvironment();
    int point = current.point();
    Matcher m = StringRegexpUtil.find(currentEnv, regexp.getData(), current.getText(), point, LispSymbol.T.equals(currentEnv.find("case-fold-search")));
    try {
      int start = m.start();
      if (start == point) {
        myBuffer = current;
        myLastMatch.clear();
        myLastMatch.add(m.start(0));
        myLastMatch.add(m.end(0));
        return LispSymbol.T;
      }
    } catch (IllegalStateException e) { //no match
      return LispSymbol.NIL;
    }
    return LispSymbol.NIL;
  }
}
