package org.jetbrains.emacs4ij.jelisp.parser;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFloat;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispNumber;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Observable;

abstract class Parser extends Observable {
  protected int myCurrentIndex = 0;
  protected String myLispCode;

  protected static final List<Character> mySeparators = Arrays.asList(']', ')', '"', ' ', ';', '\n', '\t', '\f', '(', '[');
  protected static final List<Character> myWhitespaces = Arrays.asList('\n', ' ', '\t', '\f');
  protected abstract void advance();
  protected abstract int getMyCurrentIndex();
  protected abstract char getNextChar();
  protected abstract boolean hasNextChar();
  protected abstract int getNextIndexOf (char what);
  protected abstract String extractForm(int nextSeparatorIndex);

  public abstract void append (String lispCode);

  public abstract LispObject parseLine (String lispCode, int startIndex);

  public LispObject parseLine (String lispCode) {
    return parseLine(lispCode, 0);
  }

  protected void advanceTo(int newValue) {
    myCurrentIndex = newValue;
  }

  protected char getCurrentChar() {
    return myLispCode.charAt(getMyCurrentIndex());
  }

  protected abstract int getNextIndexOfItem (List<Character> items);

  protected List<Integer> getItemsIndexes (List<Character> items) {
    List<Integer> nextIndex = new ArrayList<>();
    for (char separator : items) {
      nextIndex.add(getNextIndexOf(separator));
    }
    return nextIndex;
  }

  protected int getNextSeparatorIndex() {
    return getNextIndexOfItem(mySeparators);
  }

  protected LispNumber parseNumber () {
    int nextSeparatorIndex = getNextSeparatorIndex();
    String numberCandidate = extractForm(nextSeparatorIndex);
    try {
      int intNumber = Integer.parseInt(numberCandidate);
      advanceTo(nextSeparatorIndex);
      return new LispInteger(intNumber);
    } catch (NumberFormatException e) {
      try {
        double dblNumber = Double.parseDouble(numberCandidate);
        advanceTo(nextSeparatorIndex);
        return new LispFloat(dblNumber);
      } catch (NumberFormatException e1) {
        LispFloat lispFloat = null;
        if (numberCandidate.equals("1.0e+INF") || numberCandidate.equals("+1.0e+INF"))
          lispFloat = LispFloat.ourPositiveInfinity;
        if (numberCandidate.equals("-1.0e+INF"))
          lispFloat = LispFloat.ourNegativeInfinity;
        if (numberCandidate.equals("0.0e+NaN") || numberCandidate.equals("-0.0e+NaN"))
          lispFloat = LispFloat.ourNaN;

        if (lispFloat != null) {
          advanceTo(nextSeparatorIndex);
          return lispFloat;
        }
        return null;
      }
    }
  }

  protected LispObject parseSymbol () {
    int nextSeparatorIndex = getNextSeparatorIndex();
    String symbol = extractForm(nextSeparatorIndex);
    advanceTo(nextSeparatorIndex);
    if (StringUtil.isEmptyOrSpaces(symbol) || symbol.equals(""))
      return null;
    return new LispSymbol(symbol);
  }

  protected LispObject parseQuote(boolean isBackQuote) {
    LispObject lispObject = parseObject(isBackQuote);
    return LispList.list(new LispSymbol("quote"), lispObject);
  }

  protected LispObject parseFunctionQuote (boolean isBackQuote) {
    LispObject lispObject = parseObject(isBackQuote);
    return LispList.list(new LispSymbol("function"), lispObject);
  }

  protected LispObject parseBackQuote() {
    LispObject lispObject = parseObject(true);
    return LispList.list(new LispSymbol("\\`"), lispObject);
  }

  protected LispObject parseObject() {
    return parseObject(false);
  }

  protected abstract LispObject tryToParse (boolean isBackQuote);

  protected LispObject parseObject(boolean isBackQuote) {
    try {
      getCurrentChar();
    } catch (EndOfLineException e) {
      return LispSymbol.NIL;
    }
    return tryToParse(isBackQuote);
  }
}
