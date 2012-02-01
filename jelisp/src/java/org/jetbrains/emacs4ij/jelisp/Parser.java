package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Observable;

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

    private LispObject parseList() throws LispException {
        ArrayList<LObject> data = new ArrayList<>();
        while (true) {
            try {
                while (getCurrentChar() != ')') {
                    if ((getCurrentChar() == ' ') || (getCurrentChar() == '\n') || (getCurrentChar() == '\t')) {
                        advance();
                        continue;
                    }
                    LObject object = parseObject();
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
        return LispList.list(data);
    }

    private LispObject parseVector() throws LispException {
        LispVector vector = new LispVector();
        while (true) {
            try {
                while (getCurrentChar() != ']') {
                    if ((getCurrentChar() == ' ') || (getCurrentChar() == '\n') || (getCurrentChar() == '\t')) {
                        advance();
                        continue;
                    }
                    vector.add(parseObject());
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

    private LispObject parseSymbol () throws EndOfLineException {
        int nextSeparatorIndex = getNextSeparatorIndex();
        int currentIndex = getMyCurrentIndex();
        String symbol = myLispCode.substring(currentIndex, nextSeparatorIndex);
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

    public LispObject parseLine (String lispCode) throws LispException {
        myCurrentIndex = 0;
        myLispCode = lispCode;
        myLispCode = myLispCode.trim();
        LispObject lispObject = parseObject();
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

    private LispObject parseQuote() throws LispException {
        try {
            while (getCurrentChar() == '\'') {
                advance();
            }
        } catch (EndOfLineException ignored) {

        }
        LispObject lispObject = parseObject();
        return LispList.list(new LispSymbol("quote"), lispObject);
    }

    private LispObject parseObject() throws LispException {
        try {
            getCurrentChar();
        } catch (EndOfLineException e) {
            return LispSymbol.ourNil;
        }

        if (getCurrentChar() == '\'') {
            advance();
            return parseQuote();
        }

        if (getCurrentChar() == '"') {
            advance();
            return parseString();
        }

        if (getCurrentChar() == '(') {
            advance();
            return parseList();
        }

        if (getCurrentChar() == '[') {
            advance();
            return parseVector();
        }

        if (getCurrentChar() == '?') {
            advance();
            return parseCharacter();
        }

        if (getCurrentChar() == ';') {
            //it is a comment, skip to the end of line
            advanceTo(getNextIndexOf('\n'));
            return null;
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
