package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.EndOfLineException;

import java.util.Arrays;
import java.util.List;
import java.util.Observable;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 13/02/12
 * Time: 22:10
 * To change this template use File | Settings | File Templates.
 */
public abstract class Parser extends Observable {
    protected int myCurrentIndex = 0;
    protected String myLispCode;
    //TODO: to enum or hashmap
    protected final char[] mySeparators = new char[] {']', ')', '"', ' ', ';', '\n', '\t', '(', '['};
    protected final List<Character> myInnerSeparators = Arrays.asList('\n', ' ', '\t');
    protected final List<Character> mySpecialChars = Arrays.asList('.', ',', '?');

    protected abstract void advance();
    protected abstract int getMyCurrentIndex();
    protected abstract char getNextChar();
    protected abstract boolean hasNextChar();
    protected abstract int getNextIndexOf (char what);
    protected abstract int getNextSeparatorIndex();
    protected abstract String extractForm(int nextSeparatorIndex);

    public abstract void append (String lispCode);
    public abstract LObject parseLine (String lispCode);

    protected void advanceTo(int newValue) {
        myCurrentIndex = newValue;
    }

    protected char getCurrentChar() {
        return myLispCode.charAt(getMyCurrentIndex());
    } 

    protected LispObject parseNumber () {
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
                return LispSymbol.ourNil;
            }
        }
    }

    protected LispObject parseSymbol () {
        int nextSeparatorIndex = getNextSeparatorIndex();
        String symbol = extractForm(nextSeparatorIndex);
        
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

    protected LispObject parseQuote(boolean isBackQuote) {
        LObject lispObject = parseObject(isBackQuote);
        return LispList.list(new LispSymbol("quote"), lispObject);
    }

    protected LispObject parseBackQuote() {
        LObject lispObject = parseObject(true);
        return LispList.list(new LispSymbol("\\`"), lispObject);
    }

    protected LispObject parseComma() {
        String spec = "\\,";
        if (getCurrentChar() == '@') {
            advance();
            spec += '@';
        }
        LObject lispObject = parseObject(true);
        return LispList.list(new LispSymbol(spec), lispObject);
    }

    protected LObject parseObject() {
        return parseObject(false);
    }
    
    protected abstract LObject tryToParse (boolean isBackQuote);

    protected LObject parseObject(boolean isBackQuote) {
        try {
            getCurrentChar();
        } catch (EndOfLineException e) {
            return LispSymbol.ourNil;
        }
        return tryToParse(isBackQuote);       
    }
}
