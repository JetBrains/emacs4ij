package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import sun.plugin.javascript.navig.Array;

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
    private char[] mySeparators = new char[] {')', '"', ' '};

    private void increaseMyCurrentIndex() throws EndOfLineException {
        if (myCurrentIndex == myLispCode.length())
            throw new EndOfLineException();
        ++myCurrentIndex;
    }

    private int getMyCurrentIndex() throws EndOfLineException {
        if (myCurrentIndex == myLispCode.length())
            throw new EndOfLineException();
        return myCurrentIndex;
    }

    private void setMyCurrentIndex(int newValue) {
        myCurrentIndex = newValue;
    }

    private char getCurrentChar() throws EndOfLineException {
        return myLispCode.charAt(getMyCurrentIndex());
    }

    private LispObject parseList() throws LispException {
        LispList list = new LispList();
        try {
            while (getCurrentChar() != ')') {
                if (getCurrentChar() == ' ') {
                    increaseMyCurrentIndex();
                    continue;
                }
                list.add(parseObject());
            }
        } catch (EndOfLineException e) {
            throw new MissingClosingBracketException();
        }

        setMyCurrentIndex(getMyCurrentIndex()+1);

        if (list.isEmpty())
            return Environment.ourNilSymbol;

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
        setMyCurrentIndex(nextDoubleQuoteIndex + 1);
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
            setMyCurrentIndex(nextSeparatorIndex);
            return new LispInteger(intNumber);
        } catch (NumberFormatException e) {
            try {
                double dblNumber = Double.parseDouble(numberCandidate);
                setMyCurrentIndex(nextSeparatorIndex);
                return new LispFloat(dblNumber);
            } catch (NumberFormatException e1) {
                return Environment.ourNilSymbol;
            }
        }
    }

    private LispObject parseSymbol () {
        return Environment.ourNilSymbol;
    }

    public LispObject parseLine (String lispCode) throws LispException {
        //TODO: what to do with empty code string?
        myCurrentIndex = 0;
        myLispCode = lispCode;
        myLispCode = myLispCode.trim();
        return parseObject();
    }

    private LispObject parseQuote() throws LispException {
        // TODO: deal with ' and nothing next
        LispObject lispObject = parseObject();
        return lispObject.toLispString();
    }

    private LispObject parseObject() throws LispException {
        //TODO: stack to hold quotes, brackets and so on -- because strings and lists can be in multiple lines


        if (getCurrentChar() == '\'') {
            increaseMyCurrentIndex();
            return parseQuote();
        }

        if (getCurrentChar() == '"') {
            increaseMyCurrentIndex();
            return parseString();
        }

        if (getCurrentChar() == '(') {
            increaseMyCurrentIndex();
            return parseList();
        }

        LispObject lispObject = parseNumber();
        if (lispObject == Environment.ourNilSymbol) {
            lispObject = parseSymbol();
            if (lispObject == Environment.ourNilSymbol)
                throw new UnknownCodeBlockException();
        }

        return lispObject;
    }
}
