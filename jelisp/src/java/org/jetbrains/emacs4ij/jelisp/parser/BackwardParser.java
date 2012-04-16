package org.jetbrains.emacs4ij.jelisp.parser;

import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfFileException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.EndOfLineException;
import org.jetbrains.emacs4ij.jelisp.parser.exception.ScanException;

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

    @Override
    protected int getNextIndexOfItem(List<Character> items) {
        return Collections.max(getItemsIndexes(items));
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

    @Override
    protected int getNextIndexOf (char what) {
        return getNextIndex(what, getMyCurrentIndex());
    }

    private int getNextIndex (char what, int from) {
        int i = myLispCode.lastIndexOf(what, from);
        if (i > 0)
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
        List<Character> skipChars = new ArrayList<>(myWhitespaces);
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
        char end = myWhitespaces.contains(getCurrentChar()) ? getNextChar() : getCurrentChar();
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

