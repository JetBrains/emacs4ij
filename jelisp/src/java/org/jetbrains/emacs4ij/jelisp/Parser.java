package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 12:59 PM
 *
 * this is a parser for lisp program
 */

public class Parser {
    private int myCurrentIndex = 0;
    private String myLispCode;
    //TODO: to enum or hashmap
    private char[] mySeparators = new char[] {')', '"', ' ', ';', '\n'};

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
        LispList list = new LispList();
        try {
            while (getCurrentChar() != ')') {
                if ((getCurrentChar() == ' ') || ((getCurrentChar() == '\n'))) {
                    advance();
                    continue;
                }
                list.add(parseObject());
            }
        } catch (EndOfLineException e) {
            throw new MissingClosingBracketException();
        }

        advanceTo(getMyCurrentIndex() + 1);

        return list;
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
        int nextDoubleQuoteIndex = getNextIndexOf('"');

        if (nextDoubleQuoteIndex == myLispCode.length())
            throw new MissingClosingDoubleQuoteException();

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

                return LispSymbol.ourNilSymbol;
            }
        }
    }

    private LispObject parseSymbol () throws EndOfLineException {
        int nextSeparatorIndex = getNextSeparatorIndex();
        int currentIndex = getMyCurrentIndex();
        advanceTo(nextSeparatorIndex);
        return new LispSymbol(myLispCode.substring(currentIndex, nextSeparatorIndex));
    }

    public LispObject parseLine (String lispCode) throws LispException {
        myCurrentIndex = 0;
        myLispCode = lispCode;
        myLispCode = myLispCode.trim();
        LispObject lispObject = parseObject();
        if (lispObject == null)
            lispObject = LispSymbol.ourNilSymbol;

        try {
            getMyCurrentIndex();
        } catch (EndOfLineException ignored) {
            return lispObject;
        }
        if (getCurrentChar() == ';')
            return lispObject;
        throw new UnknownCodeBlockException();
    }

    private LispObject parseQuote() throws LispException {
        try {
            while (getCurrentChar() == '\'') {
                advance();
            }
        } catch (EndOfLineException ignored) {

        }

        LispObject lispObject = parseObject();
        /*if (lispObject instanceof LispList) {
            if (((LispList) lispObject).car() instanceof LispSymbol)
                if (((LispSymbol)(((LispList) lispObject).car())).equals(new LispSymbol("quote")))
                return lispObject;
        }*/
        return new LispList(Arrays.<LispObject>asList(new LispSymbol("quote"), lispObject));
    }

    private LispObject parseObject() throws LispException {
        try {
            getCurrentChar();
        } catch (EndOfLineException e) {
            return LispSymbol.ourNilSymbol;
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

        if (getCurrentChar() == ';') {
            //it is a comment, skip to the end of line
            advanceTo(getNextIndexOf('\n'));
            return null;
        }

        LispObject lispObject = parseNumber();
        if (lispObject == LispSymbol.ourNilSymbol) {
            lispObject = parseSymbol();
            if (lispObject == LispSymbol.ourNilSymbol)
                throw new UnknownCodeBlockException();
        }

        return lispObject;
    }
}
