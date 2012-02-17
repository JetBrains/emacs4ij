package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.exception.ScanException;

import java.util.ArrayList;
import java.util.Collections;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 4:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class BackwardParser extends Parser {

    private ForwardParser myForwardParser = new ForwardParser();

    @Override
    protected void advance() {
        if (myCurrentIndex == -1)
            throw new EndOfLineException();
        --myCurrentIndex;
    }

    @Override
    protected int getMyCurrentIndex() {
        if (myCurrentIndex == -1)
            throw new EndOfLineException();
        return myCurrentIndex;
    }

    @Override
    protected char getNextChar() {
        return myLispCode.charAt(getMyCurrentIndex() - 1);
    }

    @Override
    protected boolean hasNextChar() {
        return myCurrentIndex > 0;
    }

    private String extractSymmetricForm(char formStart, char formEnd) {
        String form = "";
        int n = 1;
        int end = getMyCurrentIndex();
        while (true) {
            try {
                advance();
                if (getCurrentChar() == formStart) {
                    n--;
                } else if (getCurrentChar() == formEnd)
                    n++;

                if (n == 0) {
                    int start = getMyCurrentIndex();
                    form = myLispCode.substring(start, end+1) + form;
                    advanceTo(start - 1);
                    if (myCurrentIndex > -1) {
                        int k = getNextSeparatorIndex();
                        form = myLispCode.substring(k + 1, start) + form;
                        advanceTo(k);
                    }
                    return form;
                }
            } catch (EndOfLineException e) {
                if (countObservers() == 0)
                    throw new ScanException("Unbalanced parentheses");
                setChanged();
                notifyObservers(new ScanException("Unbalanced parentheses"));
                clearChanged();
                end += myCurrentIndex + 1;
            }
        }
    }

    private int getNextDoubleQuoteIndex (int from) {
        int i = myLispCode.lastIndexOf('"', from);
        if (i == -1)
            return i;
        if (i != 0 && myLispCode.charAt(i - 1) == '\\')
            i = getNextDoubleQuoteIndex(i - 1);
        return i;
    }

    @Override
    protected int getNextIndexOf (char what) {
        return (what == '"') ? getNextDoubleQuoteIndex(getMyCurrentIndex()) : myLispCode.lastIndexOf(what, getMyCurrentIndex());
    }

    @Override
    protected int getNextSeparatorIndex()  {
        ArrayList<Integer> nextSeparatorIndex = new ArrayList<Integer>();
        for (char separator : mySeparators) {
            nextSeparatorIndex.add(getNextIndexOf(separator));
        }
        return Collections.max(nextSeparatorIndex);
    }

    @Override
    protected String extractForm(int nextSeparatorIndex) {
        return myLispCode.substring(nextSeparatorIndex+1, getMyCurrentIndex()+1);
    }

    @Override
    public LObject parseLine (String lispCode) {
        return parseLine(lispCode, lispCode.length()-1);
    }

    public LObject parseLine (String lispCode, int index) {
        myCurrentIndex = index;
        myLispCode = lispCode;
        LObject lispObject = parseObject();
        if (lispObject == null)
            lispObject = LispSymbol.ourNil;
        return lispObject;
    }

    @Override
    protected LObject tryToParse(boolean isBackQuote) {
        char end = getCurrentChar();
        char start = '0';

        switch (getCurrentChar()) {
            case '"':
                start = '"';
                break;
            case ')':
                start = '(';
                break;
            case ']':
                start = '[';
                break;
            case '(': case '[':
                throw new ScanException("Containing expression ends prematurely");
        }
        if (start != '0') {
            //advance();
            String form = extractSymmetricForm(start, end);
            return myForwardParser.parseLine(form);
        }
        int nextSeparatorIndex = getNextSeparatorIndex();
        String form = extractForm(nextSeparatorIndex);
        advanceTo(getMyCurrentIndex() - form.length());
        return myForwardParser.parseLine(form);
    }

    @Override
    public void append (String lispCode) {
        myLispCode = lispCode + "\n" + myLispCode;
        myCurrentIndex = lispCode.length();
    }

}

