package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.exception.ScanException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
        int end = getMyCurrentIndex() + 1;
        while (true) {
            try {
                advance();
                if (getCurrentChar() == formStart) {
                    n--;
                } else if (getCurrentChar() == formEnd)
                    n++;

                if (n == 0) {
                    int start = getMyCurrentIndex();
                    form = myLispCode.substring(start, end) + form;
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
                    throw new ScanException(JelispBundle.message("unbalanced.parentheses"));
                form = myLispCode.substring(0, end) + form;
                setChanged();
                notifyObservers(JelispBundle.message("unbalanced.parentheses"));
                clearChanged();
                end = myCurrentIndex + 1;
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
        return what == '"'
                ? getNextDoubleQuoteIndex(getMyCurrentIndex())
                : myLispCode.lastIndexOf(what, getMyCurrentIndex());
    }

    @Override
    protected int getNextSeparatorIndex()  {
        ArrayList<Integer> nextSeparatorIndex = new ArrayList<>();
        for (char separator : mySeparators) {
            nextSeparatorIndex.add(getNextIndexOf(separator));
        }
        return Collections.max(nextSeparatorIndex);
    }

    @Override
    protected String extractForm(int nextSeparatorIndex) {
        return myLispCode.substring(nextSeparatorIndex + 1, getMyCurrentIndex() + 1);
    }

    @Override
    public LispObject parseLine (String lispCode) {
        return parseLine(lispCode, lispCode.length()-1);
    }

    public LispObject parseLine (String lispCode, int index) {
        myCurrentIndex = index;
        myLispCode = lispCode;
        LispObject lispObject = parseObject();
        if (lispObject == null)
            lispObject = LispSymbol.ourNil;
        return lispObject;
    }

    private void skipSpacesAndEmptyComments () {
        char last = 0;
        List<Character> skipChars = new ArrayList<>(myInnerSeparators);
        skipChars.add(';');
        while (skipChars.contains(getCurrentChar())) {
            int i = myCurrentIndex;
            for (; i > -1 && skipChars.contains(myLispCode.charAt(i)); i--) {
                if (myLispCode.charAt(i) != ';')
                    last = myLispCode.charAt(i);
            }
            String data = i == -1 ? "" : myLispCode.substring(0, i + 1);
            if (StringUtil.isEmpty(data)) {
                if (countObservers() == 0)
                    throw new EndOfFileException();
                setChanged();
                notifyObservers(new EndOfFileException());
                clearChanged();
            } else {
                myLispCode = last == 0
                        ? data
                        : data + last;
                myCurrentIndex = myLispCode.length() - 1;
                break;
            }
        }
    }

    @Override
    protected LispObject tryToParse(boolean isBackQuote) {
        skipSpacesAndEmptyComments();
        char end = myInnerSeparators.contains(getCurrentChar()) ? getNextChar() : getCurrentChar();
        char start = '0';
        switch (end) {
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
                throw new ScanException(JelispBundle.message("unexpected.expression.end"));
        }
        if (start != '0') {
            if (end != getCurrentChar()) advance();
            String form = extractSymmetricForm(start, end);
            return myForwardParser.parseLine(form);
        }
        if (end != getCurrentChar()) {
            end = getCurrentChar();
            advance();
        } else end = 0;
        int nextSeparatorIndex = getNextSeparatorIndex();
        String form = extractForm(nextSeparatorIndex);
        advanceTo(getMyCurrentIndex() - form.length());
        return myForwardParser.parseLine(end == 0 ? form : form + end);
    }

    @Override
    public void append (String lispCode) {
        myLispCode = lispCode + "\n";
        myCurrentIndex = lispCode.length();
    }

}

