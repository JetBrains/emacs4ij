package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 12:59 PM
 *
 * this is a parser for lisp program
 */

public class Parser extends Observable {
    private int myCurrentIndex = 0;
    private String myLispCode;
    //TODO: to enum or hashmap
    private char[] mySeparators = new char[] {']', ')', '"', ' ', ';', '\n', '\t'};

    private void advance() throws EndOfLineException {
        if (myCurrentIndex == myLispCode.length())
            throw new EndOfLineException();
        ++myCurrentIndex;
    }

    private int getMyCurrentIndex() throws EndOfLineException {
        if (myCurrentIndex == myLispCode.length())
            throw new EndOfLineException();
        return myCurrentIndex;
    }

    private void advanceTo(int newValue) {
        myCurrentIndex = newValue;
    }

    private char getCurrentChar() throws EndOfLineException {
        return myLispCode.charAt(getMyCurrentIndex());
    }

    private char getNextChar() {
        return myLispCode.charAt(getMyCurrentIndex() + 1);
    }

    private boolean hasNextChar() {
        return myCurrentIndex < myLispCode.length() - 1;
    }

    private List<Character> myListElementsSeparators = Arrays.asList('\n', ' ', '\t');

    private void skipListSeparators() throws LispException {
        while (true) {
            if (myListElementsSeparators.contains(getCurrentChar())) {
                advance();
                continue;
            }
            break;
        }
    }

    private LObject parseList(boolean isBackQuote) throws LispException {
        ArrayList<LObject> data = new ArrayList<>();
        boolean makeList = true;
        boolean wasCons = false;
        while (true) {
            try {
                while (true) {
                    skipListSeparators();
                    if (getCurrentChar() == ')')
                        break;
                    if (!makeList)
                        throw new InvalidReadSyntax(". in wrong context");
                    if (getCurrentChar() == '.') {
                        if (!hasNextChar())
                            throw new InvalidReadSyntax(".");
                        if (myListElementsSeparators.contains(getNextChar())) {
                            advance();
                            skipListSeparators();
                            if (getCurrentChar() == ')')
                                throw new InvalidReadSyntax(")");
                            if (data.size() == 0) {
                                makeList = false;
                                LObject object = parseObject(isBackQuote);
                                if (object != null)
                                    data.add(object);
                                continue;
                            }
                            LObject car = data.get(data.size()-1);
                            LObject cdr = parseObject(isBackQuote);
                            while (cdr == null) {
                                advance();
                                if (getCurrentChar() == ')')
                                    throw new InvalidReadSyntax(")");
                                cdr = parseObject(isBackQuote);
                            }
                            //while (cdr != null) {
                            data.set(data.size()-1, LispList.cons(car, cdr));
                            wasCons = true;
                            skipListSeparators();
                            if (getCurrentChar() != ')')
                                throw new InvalidReadSyntax(". in wrong context");
                            break;
                            // } else
                            //     throw new RuntimeException("Cons cdr is null!");
                        }
                    }
                    LObject object = parseObject(isBackQuote);
                    if (object != null)
                        data.add(object);
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
        if (makeList && (!(data.size() == 1 && wasCons)))// && !(data.size() == 1 && extraBrackets && data.get(0) instanceof LispList)))
            return LispList.list(data);
            
        return data.get(0);
    }

    private LispObject parseVector(boolean isBackQuote) throws LispException {
        LispVector vector = new LispVector();
        while (true) {
            try {
                while (getCurrentChar() != ']') {
                    if ((getCurrentChar() == ' ') || (getCurrentChar() == '\n') || (getCurrentChar() == '\t')) {
                        advance();
                        continue;
                    }
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

    private int getNextDoubleQuoteIndex (int from) {
        int i = myLispCode.indexOf('"', from);
        if (i == -1)
            return i;
        if (i != 0)
            if (myLispCode.charAt(i-1) == '\\')
                if (i + 1 != myLispCode.length()) {
                    i = getNextDoubleQuoteIndex(i+1);
                } else {
                    return -1;
                }
        return i;
    }

    private int getNextIndexOf (char what) throws EndOfLineException {
        int i = (what == '"') ? getNextDoubleQuoteIndex(getMyCurrentIndex()) : myLispCode.indexOf(what, getMyCurrentIndex());
        return ((i == -1) ? myLispCode.length() : i);
    }

    private LispString parseString() throws MissingClosingDoubleQuoteException, EndOfLineException {
        int nextDoubleQuoteIndex;
        while (true) {
            nextDoubleQuoteIndex = getNextIndexOf('"');
            if (nextDoubleQuoteIndex == myLispCode.length()) {
                if (countObservers() == 0)
                    throw new MissingClosingDoubleQuoteException();
                setChanged();
                notifyObservers(new MissingClosingDoubleQuoteException());
                clearChanged();
                continue;
            }
            break;
        }
        String string = myLispCode.substring(getMyCurrentIndex(), nextDoubleQuoteIndex);
        advanceTo(nextDoubleQuoteIndex + 1);
        return new LispString(string);
    }

    private int getNextSeparatorIndex() throws EndOfLineException {
        ArrayList<Integer> nextSeparatorIndex = new ArrayList<Integer>();
        for (char separator : mySeparators) {
            nextSeparatorIndex.add(getNextIndexOf(separator));
        }
        return Collections.min(nextSeparatorIndex);
    }

    private LispObject parseNumber () throws EndOfLineException {
        int nextSeparatorIndex = getNextSeparatorIndex();
        String numberCandidate = myLispCode.substring(getMyCurrentIndex(), nextSeparatorIndex);
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

                return LispSymbol.ourNil;
            }
        }
    }

    private List<Character> mySpecialChars = Arrays.asList('.', ',', '?');

    private LispObject parseSymbol () throws EndOfLineException {
        int nextSeparatorIndex = getNextSeparatorIndex();
        int currentIndex = getMyCurrentIndex();
        String symbol = myLispCode.substring(currentIndex, nextSeparatorIndex);
        /*for (int i = 0; i < symbol.length(); ++i) {
            char c = symbol.charAt(i);
            if (c == '\\') {
                if (i < symbol.length() - 1 &&
                        (mySpecialChars.contains(symbol.charAt(i+1)) || symbol.charAt(i+1) == '\\')) {
                    i++;
                    continue;
                }
                symbol = symbol.substring(0, i) + symbol.substring(i+1);
                i--;
                continue;
            }
            if (mySpecialChars.contains(symbol.charAt(i))) {
                symbol = symbol.substring(0, i) + '\\' + symbol.substring(i);
                i++;
            }
        }*/
        advanceTo(nextSeparatorIndex);
        return new LispSymbol(symbol);
    }

    private LispInteger parseCharacter () throws EndOfLineException {
        int nextSeparatorIndex = getNextSeparatorIndex();
        int currentIndex = getMyCurrentIndex();
        String character = myLispCode.substring(currentIndex, nextSeparatorIndex);
        int answer = -1;
        if (character.charAt(0) == '\\') {
            if (character.length() == 1) {
                throw new RuntimeException("You must have forgotten the <space> between <?\\ > character and next code!");
            }
            switch (character.charAt(1)) {
                case '^':
                    //Ctrl+following
                    switch (character.charAt(2)) {
                        case 'j':
                            answer = '\n';
                            break;
                        default:
                            throw new RuntimeException("Character <?\\^...> not implemented yet.");
                    }
                    break;
                case 'C':
                    if (character.length() < 3) {
                        throw new RuntimeException("Error in syntax: " + character);
                    }
                    if (character.charAt(2) == '-') {
                        switch (character.charAt(3)) {
                            case 'j':
                                answer = '\n';
                                break;
                            default:
                                throw new RuntimeException("Character <?\\C-...> not implemented yet.");
                        }
                        break;
                    }
                    throw new RuntimeException("Unknown special character " + character);
                case 'a':  //control-g, C-g
                    throw new RuntimeException("I don't know " + character + " yet.");
                    //  break;
                case 'b': //backspace, <BS>, C-h
                    throw new RuntimeException("I don't know " + character + " yet.");
                    // break;
                case 't': //tab, <TAB>, C-i
                    answer = '\t';
                    //throw new RuntimeException("I don't know " + character + " yet.");
                    break;
                case 'n': //newline, C-j
                    answer = '\n';
                    break;
                case 'v': //vertical tab, C-k
                    //throw new RuntimeException("I don't know " + character + " yet.");
                    break;
                case 'f': //formfeed character, C-l
                    throw new RuntimeException("I don't know " + character + " yet.");
                    //break;
                case 'r': //carriage return, <RET>, C-m
                    throw new RuntimeException("I don't know " + character + " yet.");
                    //break;
                case 'e': //escape character, <ESC>, C-[
                    throw new RuntimeException("I don't know " + character + " yet.");
                    //break;
                case 's': //space character, <SPC>
                    answer = ' ';
                    // throw new RuntimeException("I don't know " + character + " yet.");
                    break;
                case '\\': //backslash character, \
                    answer = '\\';
                    //throw new RuntimeException("I don't know " + character + " yet.");
                    break;
                case 'd': //delete character, <DEL>
                    throw new RuntimeException("I don't know " + character + " yet.");
                    //break;
                default:
                    throw new RuntimeException("I don't expect " + character + " here!");

                    /*
                   ?\a ⇒ 7                 ; control-g, C-g
        ?\b ⇒ 8                 ; backspace, <BS>, C-h
        ?\t ⇒ 9                 ; tab, <TAB>, C-i
        ?\n ⇒ 10                ; newline, C-j
        ?\v ⇒ 11                ; vertical tab, C-k
        ?\f ⇒ 12                ; formfeed character, C-l
        ?\r ⇒ 13                ; carriage return, <RET>, C-m
        ?\e ⇒ 27                ; escape character, <ESC>, C-[
        ?\s ⇒ 32                ; space character, <SPC>
        ?\\ ⇒ 92                ; backslash character, \
        ?\d ⇒ 127               ; delete character, <DEL>
                    */

            }


        } else {
            answer = character.charAt(0);
        }

        advanceTo(nextSeparatorIndex);
        return new LispInteger(answer);
    }

    public LObject parseLine (String lispCode) throws LispException {
        myCurrentIndex = 0;
        myLispCode = lispCode;
        myLispCode = myLispCode.trim();
        LObject lispObject = parseObject();
        if (lispObject == null)
            lispObject = LispSymbol.ourNil;

        try {
            getMyCurrentIndex();
        } catch (EndOfLineException ignored) {
            return lispObject;
        }
        if (getCurrentChar() == ';')
            return lispObject;
        throw new UnknownCodeBlockException(myLispCode.substring(getMyCurrentIndex()));
    }

    private LispObject parseQuote(boolean isBackQuote) throws LispException {
        LObject lispObject = parseObject(isBackQuote);
        return LispList.list(new LispSymbol("quote"), lispObject);
    }

    private LispObject parseBackQuote() throws LispException {
        LObject lispObject = parseObject(true);
        return LispList.list(new LispSymbol("\\`"), lispObject);
    }

    private LispObject parseComma() throws LispException {
        String spec = "\\,";
        if (getCurrentChar() == '@') {
            advance();
            spec += '@';
        }
        LObject lispObject = parseObject(true);
        return LispList.list(new LispSymbol(spec), lispObject);
    }

    private LObject parseObject() throws LispException {
        return parseObject(false);
    }

    private LObject parseObject(boolean isBackQuote) throws LispException {
        try {
            getCurrentChar();
        } catch (EndOfLineException e) {
            return LispSymbol.ourNil;
        }

        switch (getCurrentChar()) {
            case '\'':
                advance();
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
                advance();
                return parseCharacter();
            case ';':
                //it is a comment, skip to the end of line
                advanceTo(getNextIndexOf('\n'));
                return null;
            case '`':
                advance();
                return parseBackQuote();
            case ',':
                if (!isBackQuote)
                    break;
                advance();
                return parseComma();
            case '.':
                if (hasNextChar()) {
                    if (myListElementsSeparators.contains(getNextChar()))
                        throw new InvalidReadSyntax(".");
                }
        }

        LispObject lispObject = parseNumber();
        if (lispObject == LispSymbol.ourNil) {
            lispObject = parseSymbol();
            if (lispObject == LispSymbol.ourNil)
                throw new UnknownCodeBlockException(myLispCode.substring(myCurrentIndex, getNextIndexOf('\n')));
        }

        return lispObject;
    }

    public void append (String lispCode) {
        myLispCode += lispCode;
    }

}