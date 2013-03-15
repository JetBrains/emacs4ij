package org.jetbrains.emacs4ij.jelisp.parser;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.LispVector;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.InvalidReadSyntax;
import org.jetbrains.emacs4ij.jelisp.parser.exception.InvalidReadSyntaxDot;
import org.jetbrains.emacs4ij.jelisp.parser.exception.InvalidUnicodeCharacterException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.MissingClosingBracketException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.MissingClosingDoubleQuoteException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ScanException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.UnknownCodeBlockException;
import org.jetbrains.emacs4ij.jelisp.subroutine.TextProperties;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public final class ForwardParser extends Parser {
  @Override
  protected void advance() {
    if (myCurrentIndex == myLispCode.length())
      throw new EndOfLineException();
    ++myCurrentIndex;
  }

  @Override
  protected int getMyCurrentIndex() {
    if (myCurrentIndex == myLispCode.length())
      throw new EndOfLineException();
    return myCurrentIndex;
  }

  @Override
  protected char getNextChar() {
    return myLispCode.charAt(getMyCurrentIndex() + 1);
  }

  @Override
  protected boolean hasNextChar() {
    return myCurrentIndex < myLispCode.length() - 1;
  }

  @Override
  protected int getNextIndexOfItem(List<Character> items) {
    return Collections.min(getItemsIndexes(items));
  }

  private void skipWhitespaces() {
    while (true) {
      if (myWhitespaces.contains(getCurrentChar())) {
        advance();
        continue;
      }
      break;
    }
  }

  private LispObject parseList(boolean isBackQuote) {
    ArrayList<LispObject> data = new ArrayList<>();
    boolean makeList = true;
    boolean wasCons = false;
    boolean wasDot = false;
    while (true) {
      try {
        while (true) {
          skipWhitespaces();
          if (getCurrentChar() == ')') {
            if (wasDot)
              throw new InvalidReadSyntax(")");
            break;
          }
          if (!makeList)
            throw new InvalidReadSyntax(JelispBundle.message("dot.in.wrong.context"));
          if (getCurrentChar() == '.' || wasDot) {
            if (!hasNextChar() && !wasDot) {
              wasDot = true;
              throw new InvalidReadSyntaxDot();
            }
            if ((hasNextChar() && !Character.isDigit(getNextChar())) || wasDot) { //wasDot in this case means there was line break before dot, so the dot doesn't belong to lisp float
              if (!wasDot) {
                advance();
                skipWhitespaces();
              }
              if (getCurrentChar() == ')')
                throw new InvalidReadSyntax(")");
              if (data.size() == 0) {
                makeList = false;
                LispObject object = parseObject(isBackQuote);
                if (object != null)
                  data.add(object);
                continue;
              }
              LispObject car = data.get(data.size()-1);
              LispObject cdr = parseObject(isBackQuote);
              while (cdr == null) {
                advance();
                if (getCurrentChar() == ')')
                  throw new InvalidReadSyntax(")");
                cdr = parseObject(isBackQuote);
              }
              data.set(data.size()-1, LispList.cons(car, cdr));
              wasCons = true;
              wasDot = false;
              skipWhitespaces();
              if (getCurrentChar() != ')')
                throw new InvalidReadSyntax(JelispBundle.message("dot.in.wrong.context"));
              break;
            }
            wasDot = false;
          }
          LispObject object = parseObject(isBackQuote);
          if (object != null) {
            data.add(object);
          }
        }
        break;
      } catch (EndOfLineException | InvalidReadSyntaxDot e) {
        LispException exception = (LispException) (e instanceof InvalidReadSyntaxDot
            ? e
            : new MissingClosingBracketException());
        if (countObservers() == 0)
          throw exception;
        setChanged();
        notifyObservers(exception);
        clearChanged();
      }
    }
    advanceTo(getMyCurrentIndex() + 1);
    if (makeList && (!(data.size() == 1 && wasCons))) {
      if (wasCons)
        return LispList.listAsIs(data);
      return LispList.list(data);
    }
    return data.get(0);
  }

  private LispObject parseVector(boolean isBackQuote) {
    LispVector vector = new LispVector();
    while (true) {
      try {
        while (true) {
          skipWhitespaces();
          if (getCurrentChar() == ']')
            break;
          vector.add(parseObject(isBackQuote));
        }
        break;
      } catch (EndOfLineException e) {
        if (countObservers() == 0)
          throw new MissingClosingBracketException();
        setChanged();
        notifyObservers(new MissingClosingBracketException());
        clearChanged();
      }
    }
    advanceTo(getMyCurrentIndex() + 1);
    return vector;
  }

  private int getNextIndex (char what, int from) {
    int i = myLispCode.indexOf(what, from);
    if (i == -1)
      return myLispCode.length();
    if (i != 0)
      if (myLispCode.charAt(i - 1) == '\\') {
        int slashCount = 1;
        for (int j = i - 2; j > 0 && myLispCode.charAt(j) == '\\'; --j, ++slashCount) {}
        if (slashCount % 2 == 1) {
          i = getNextIndex(what, i + 1);
        } else {
          return i;
        }
      }
    return i;
  }

  @Override
  protected int getNextIndexOf (char what) {
    if (myCurrentIndex == myLispCode.length())
      return myLispCode.length();
    int i = getNextIndex(what, getMyCurrentIndex());
    return i == -1 ? myLispCode.length() : i;
  }

  @Override
  protected String extractForm(int nextSeparatorIndex) {
    return myCurrentIndex < 0 || myCurrentIndex >= myLispCode.length()
        ? ""
        : myLispCode.substring(getMyCurrentIndex(), nextSeparatorIndex);
  }

  private LispString parseString() {
    int nextDoubleQuoteIndex;
    String data = "";
    while (true) {
      nextDoubleQuoteIndex = getNextIndex('"', myCurrentIndex);
      data += extractForm(nextDoubleQuoteIndex);
      if (nextDoubleQuoteIndex == myLispCode.length()) {
        if (countObservers() == 0)
          throw new MissingClosingDoubleQuoteException();
        setChanged();
        notifyObservers(new MissingClosingDoubleQuoteException());
        clearChanged();
        data += '\n';
        continue;
      }
      break;
    }
    advanceTo(nextDoubleQuoteIndex + 1);
    return new LispString(data);
  }

  private static int convert(char c) {
    char ch = (Character.toLowerCase(c) == 'a') ? c : Character.toLowerCase(c);
    switch (ch) {
      case 'a':case '7':  //control-g, C-g  == Quit
        return 7;
      case 'b': case '\b'://backspace, <BS>, C-h
        return '\b';
      case 't': case '\t': //tab, <TAB>, C-i
        return '\t';
      case 'n': case '\n': //newline, C-j
        return '\n';
      case 'v': case 11: //vertical tab, C-k
        return 11;
      case 'f': case '\f': //formfeed character, C-l
        return '\f';
      case 'r': case '\r': //carriage return, <RET>, C-m
        return '\r';
      case 'e': case 27: //escape character, <ESC>, C-[
        return 27;
//            case 's': //space character, <SPC>
//                return ' ';
      case '\\': //backslash character, \
        return '\\';
      case 'd': case 127: //delete character, <DEL>
        return 127;
      default:
        return -1;
    }
  }

  private static int ctrlConvert (char c, boolean asIs) {
    switch (Character.toLowerCase(c)) {
      case 'g':
        return convert('a');
      case 'h':
        return convert('b');
      case 'i':
        return convert('t');
      case 'j':
        return convert('n');
      case 'k':
        return convert('v');
      case 'l':
        return convert('f');
      case 'm':
        return convert('r');
      case '[':
        return convert('e');
      case '?':
        return 127;
      default:
        if (c <= '9' && c >= '0')
          return c;
        int a = Character.toUpperCase(c) - 64;
        if (a >= 0 && a < 128)
          return a;
        if (asIs || c < 33)
          return c;
        if (c == 34)
          throw new ScanException(JelispBundle.message("invaild.modifier.in.string"));
        if (c > 32 && c < 65)
          return c;
        return -1;
    }
  }

  private static class Char {
    public enum Modifier {M, C, S, H, s, A;
      public static int indexOf (Modifier m) {
        return Arrays.asList(values()).indexOf(m);
      }
    }
    private char myModifiers[] = new char[] {'0', '0', '0', '0', '0', '0'}; //MCSHsA
    private int myKey = -1;
    private int myCtrlCount = 0;
    private boolean isAsIs = false;
    public Char() {}

    public void setModifier(Modifier modifier) {
      myModifiers[Modifier.indexOf(modifier)] = '1';
      if (modifier == Modifier.C)
        myCtrlCount++;
    }

    public void setKey(int myKey, boolean asIs) {
      this.myKey = myKey;
      isAsIs = asIs;
    }

    public Integer toInteger() {
      if (myCtrlCount != 0) {
        int k = ctrlConvert((char)myKey, isAsIs);
        if (k < 0)
          return null;
        if (myKey != k) {
          myModifiers[1] = myCtrlCount == 1 ? '0' : '1';
          myKey = k;
        }
      }
      String number = new String(myModifiers);
      String key = Integer.toBinaryString(myKey);
      for (int i = 6; i != 27 - key.length() + 1; ++i)
        number += '0';
      return Integer.valueOf(number.concat(key), 2);
    }
  }

  private void setCharKey (Char c, @Nullable Integer key, boolean asIs) {
    if (myLispCode.length() > myCurrentIndex + 1 && !mySeparators.contains(getNextChar()) // && getNextChar() != ']')
        && getCurrentChar() != ' ' && getNextChar() != '.')
      throw new InvalidReadSyntax("?");
//        int ch = key == null ? Character.toLowerCase(getCurrentChar()) : key; //todo: not in ascii?
    int ch = key == null ? getCurrentChar() : key;
    c.setKey(ch, asIs);
    advanceTo(myCurrentIndex + 1);
  }

  private void setCharKey (Char c, @Nullable Integer key) {
    setCharKey(c, key, false);
  }

  private boolean nextFourCharsAreDigits () {
    for (int i = myCurrentIndex + 1; i < myCurrentIndex + 5; i++) {
      if (!Character.isDigit(myLispCode.charAt(i)))
        return false;
    }
    return true;
  }

  private LispObject parseCharacter () {
    Char c = new Char();
    while (true) {
      advance();
      if (getCurrentChar() != '\\' ) {
        setCharKey(c, null);
        break;
      }
      advance();
      int spec = -1;
      if (!(getCurrentChar() == '7' && hasNextChar() && Character.isDigit(getNextChar())))
        spec = convert(getCurrentChar());
      if (Character.toLowerCase(getCurrentChar()) == 'u') { //unicode
        if (myCurrentIndex + 5 <= myLispCode.length() && nextFourCharsAreDigits()) {
          spec = Integer.parseInt(myLispCode.substring(myCurrentIndex + 1, myCurrentIndex + 5), 16);
          advanceTo(myCurrentIndex + 4);
        } else
          throw new InvalidUnicodeCharacterException();
      }
      if (spec != -1) {
        setCharKey(c, spec, true);
        break;
      }
      int next = hasNextChar() ? getNextChar() : -1;
      Char.Modifier m;
      try {
        m = Char.Modifier.valueOf(Character.toString(getCurrentChar()));
        if (next != '-' && getCurrentChar() != 's')
          throw new ScanException(JelispBundle.message("invalid.esc.char.syntax"));
      } catch (IllegalArgumentException e) {
        m = null;
      }

      if (m == null && getCurrentChar() == '^') {
        m = Char.Modifier.C;
        myCurrentIndex--;
      }
      if (m != null) {
        if (m == Char.Modifier.s && next != '-') {
          setCharKey(c, (int)' ');
          break;
        }
        c.setModifier(m);
        advance();
      } else if (getCurrentChar() == '^') {
        //skip
      } else if (getCurrentChar() <= '7' && getCurrentChar() >= '0') { //octal, up to 3 digits
        int nextSeparatorIndex = getNextSeparatorIndex();
        String octal = extractForm(nextSeparatorIndex);
        if (octal.length() > 3)
          throw new InvalidReadSyntax("?");
        try {
          int n = Integer.valueOf(octal, 8);
          advanceTo(nextSeparatorIndex-1);
          setCharKey(c, n);
          break;
        } catch (NumberFormatException e) {
          throw new InvalidReadSyntax("?");
        }
      } else if (getCurrentChar() == 'x') {//hex
        advance();
        int nextSeparatorIndex = getNextSeparatorIndex();
        String hex = extractForm(nextSeparatorIndex);
        try {
          int n = Integer.valueOf(hex, 16);
          advanceTo(nextSeparatorIndex-1);
          setCharKey(c, n);
          break;
        } catch (NumberFormatException e) {
          //todo: value = -1 when all symbols are ok but the string is too long
          throw new InvalidReadSyntax("?");
        }
      }
      else {
        setCharKey(c, (int)Character.toLowerCase(getCurrentChar()), true);
        break;
      }
    }
    Integer n = c.toInteger();
    return n == null ? LispSymbol.NIL : new LispInteger(n);
  }

  @Override
  public LispObject parseLine (String lispCode, int startIndex) {
    myCurrentIndex = startIndex;
    myLispCode = lispCode;
    return parseNext();
  }

  public LispObject parseNext() {
    try {
      skipWhitespaces();
    } catch (EndOfLineException e) {
      return null;
    }
    LispObject lispObject = parseObject();
    try {
      skipWhitespaces();
      getMyCurrentIndex();
      if (getCurrentChar() == ';')
        myCurrentIndex = myLispCode.length();
    } catch (EndOfLineException ignored) {
    }
    return lispObject;
  }

  public boolean isFinished() {
    return myCurrentIndex >= myLispCode.length();
  }

  private LispInteger parseInteger (int radix) {
    try {
      advance();
      int from = myCurrentIndex;
      if (getCurrentChar() == '+' || getCurrentChar() == '-')
        advance();
      while (myCurrentIndex < myLispCode.length() && Character.isLetterOrDigit(getCurrentChar())) {
        advance();
      }
      String integer = myLispCode.substring(from, myCurrentIndex);
      return new LispInteger(Integer.parseInt(integer, radix));
    } catch (EndOfLineException | NumberFormatException e) {
      throw new InvalidReadSyntax("integer, radix " + radix);
    }
  }

  protected LispObject parseComma() {
    String spec = "\\,";
    if (getCurrentChar() == '@') {
      advance();
      spec += '@';
    }
    skipWhitespaces();
    LispObject lispObject = parseObject(true);
    return LispList.list(new LispSymbol(spec), lispObject);
  }

  @Override
  protected LispObject tryToParse(boolean isBackQuote) {
    switch (getCurrentChar()) {
      case '#':
        if (!hasNextChar())
//                    throw new InvalidReadSyntax("#");
          throw new UnknownCodeBlockException(myLispCode.substring(myCurrentIndex));
        advance();
        switch (getCurrentChar()) {
          case '\'':
            advance();
            return parseFunctionQuote(isBackQuote);
          case '(':
            advance();
            return parseStringWithTextProperties();
          case 'b':case'B':
            return parseInteger(2);
          case 'o':case'O':
            return parseInteger(8);
          case 'x':case'X':
            return parseInteger(16);
          default: //integer with any radix
            try {
              if (!Character.isDigit(getCurrentChar()))
                throw new InvalidReadSyntax("#");
              String radix = Character.toString(getCurrentChar());
              advance();
              if (Character.isDigit(getCurrentChar())) {
                radix += Character.toString(getCurrentChar());
                advance();
              }
              if (getCurrentChar() == 'r') {
                return parseInteger(Integer.parseInt(radix));
              } else throw new InvalidReadSyntax("#");
            } catch (EndOfLineException e) {
              throw new InvalidReadSyntax("#");
            }
        }
      case '\'':
        advance();
        skipWhitespaces();
        return parseQuote(isBackQuote);
      case '"':
        advance();
        return parseString();
      case '(':
        advance();
        return parseList(isBackQuote);
      case '[':
        advance();
        return parseVector(isBackQuote);
      case '?':
        return parseCharacter();
      case ';':
        advanceTo(getNextIndexOf('\n'));
        return null;
      case '`':
        advance();
        return parseBackQuote();
      case ',':
        if (!isBackQuote)
          break;
        advance();
        skipWhitespaces();
        return parseComma();
      case '.':
        if (hasNextChar()) {
          if (myWhitespaces.contains(getNextChar()))
            throw new InvalidReadSyntaxDot();
        }
    }
    LispObject lispObject = parseNumber();
    if (lispObject == null) {
      lispObject = parseSymbol();
      if (lispObject == null)
        throw new ScanException(JelispBundle.message("unexpected.expression.end"));
    }
    return lispObject;
  }

  @Override
  public void append (String lispCode) {
    myLispCode = lispCode;
    myCurrentIndex = 0;
  }

  public String getCode () {
    return myLispCode;
  }

  /**
   * for read-from-string only,
   * @see org.jetbrains.emacs4ij.jelisp.subroutine.BString
   */
  public int getCurrentIndex () {
    return myCurrentIndex;
  }

  private LispString parseStringWithTextProperties() {
    LispObject object = parseList(false);
    if (!(object instanceof LispList))
      throw new InvalidReadSyntax("#");
    List<LispObject> list = ((LispList) object).toLispObjectList();
    if (list.isEmpty() || !(list.get(0) instanceof LispString))
      throw new InvalidReadSyntax("#");
    LispString string = (LispString) list.get(0);
    int index = 1;
    while (index < list.size()) {
      if (index + 2 >= list.size())
        throw new InvalidReadSyntax("Invalid string property list");
      TextProperties.addTextProperties(list.get(index), list.get(index + 1), list.get(index + 2), string);
      index += 3;
    }
    return string;
  }
}
