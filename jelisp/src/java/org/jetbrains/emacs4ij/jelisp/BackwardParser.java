package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.exception.ScanException;
import org.jetbrains.emacs4ij.jelisp.exception.UnknownCodeBlockException;

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
        while (true) {
            int end = getMyCurrentIndex();
            int start;
            while (true) {
                int falseEnd = myLispCode.lastIndexOf(formEnd, end-1);
                start = myLispCode.lastIndexOf(formStart, end-1);
                if (falseEnd > start) {
                    end = start;
                } else {
                    end = getMyCurrentIndex();
                    break;
                }
            }
            if (start == -1) {
                if (countObservers() == 0)
                    throw new ScanException("Unbalanced parentheses");
                form = form + myLispCode.substring(0, end + 1);
                setChanged();
                notifyObservers(new ScanException("Unbalanced parentheses"));
                clearChanged();
            } else {
                form = myLispCode.substring(start, end + 1) + form;
                advanceTo(start - 1);
                if (start > 0) {
                    int n = getNextSeparatorIndex();
                    form = myLispCode.substring(n + 1, start) + form;
                    advanceTo(n);
                }
                return form;
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
        myLispCode = myLispCode.trim();

        String tail = "";
        int k = myLispCode.lastIndexOf(';', myCurrentIndex);
        while (k != -1) {
            int j = myLispCode.lastIndexOf('\n', myCurrentIndex);
            int n = myLispCode.lastIndexOf("\"", myCurrentIndex);
            if (n == -1 || n < k) {
                if (j == -1 || j < k ) {
                    myCurrentIndex = k - 1;
                    myLispCode = myLispCode.substring(0, k);
                }
                else {
                    String head = myLispCode.substring(0, k);
                    tail = myLispCode.substring(j, myCurrentIndex+1) + tail;
                    myLispCode = head;
                    myCurrentIndex = k - 1;
                }
            } else
                break;
            k = myLispCode.lastIndexOf(';', myCurrentIndex);
        }

        myLispCode += tail;
        myCurrentIndex = myLispCode.length()-1;

        LObject lispObject = parseObject();
        if (lispObject == null)
            lispObject = LispSymbol.ourNil;

        try {
            getMyCurrentIndex();
        } catch (EndOfLineException ignored) {
            return lispObject;
        }
        throw new UnknownCodeBlockException(myLispCode.substring(0, getMyCurrentIndex()));
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
        myCurrentIndex += lispCode.length() - 1;
    }

}

