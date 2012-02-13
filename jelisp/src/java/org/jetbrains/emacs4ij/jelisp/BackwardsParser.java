package org.jetbrains.emacs4ij.jelisp;

import com.google.common.collect.Lists;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 4:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class BackwardsParser extends Observable {
    private int myCurrentIndex = 0;
    private String myLispCode;
    //TODO: to enum or hashmap
    private char[] mySeparators = new char[] {'[', '(', '"', ' ', ';', '\t', '\n'};

    private void advance() {
        if (myCurrentIndex == 0)
            throw new LineBeginningException();
        --myCurrentIndex;
    }

    private int getMyCurrentIndex() {
        if (myCurrentIndex == -1)
            throw new LineBeginningException();
        return myCurrentIndex;
    }

    private void advanceTo(int newValue) {
        myCurrentIndex = newValue;
    }

    private char getCurrentChar() {
        return myLispCode.charAt(getMyCurrentIndex());
    }

    private char getNextChar() {
        return myLispCode.charAt(getMyCurrentIndex() - 1);
    }

    private boolean hasNextChar() {
        return myCurrentIndex > 0;
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

    private LObject parseList(boolean isBackQuote)  {
        ArrayList<LObject> data = new ArrayList<>();
        boolean makeList = true;
        boolean wasCons = false;
        while (true) {
            try {
                while (true) {
                    skipListSeparators();
                    if (getCurrentChar() == '(')
                        break;
                    if (!makeList)
                        throw new InvalidReadSyntax(". in wrong context");
                    if (getCurrentChar() == '.') {
                        if (!hasNextChar())
                            throw new InvalidReadSyntax(".");
                        if (myListElementsSeparators.contains(getNextChar())) {
                            advance();
                            skipListSeparators();
                            if (getCurrentChar() == '(')
                                throw new InvalidReadSyntax(")");
                            if (data.size() == 0) {
                                makeList = false;
                                LObject object = parseObject(isBackQuote);
                                if (object != null)
                                    data.add(object);
                                continue;
                            }
                            LObject cdr = data.get(data.size()-1);
                            LObject car = parseObject(isBackQuote);
                            while (car == null) {
                                advance();
                                if (getCurrentChar() == '(')
                                    throw new InvalidReadSyntax(")");
                                car = parseObject(isBackQuote);
                            }
                            //while (cdr != null) {
                            data.set(data.size()-1, LispList.cons(car, cdr));
                            wasCons = true;
                            skipListSeparators();
                            if (getCurrentChar() != '(')
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
            } catch (LineBeginningException e) {
                if (countObservers() == 0)
                    throw new MissingClosingBracketException();
                setChanged();
                notifyObservers(new MissingClosingBracketException());
                clearChanged();
            }
        }
        advanceTo(getMyCurrentIndex() - 1);
        if (makeList && (!(data.size() == 1 && wasCons)))// && !(data.size() == 1 && extraBrackets && data.get(0) instanceof LispList)))
            return LispList.list(Lists.reverse(data));

        return data.get(0);
    }

    private LispObject parseVector(boolean isBackQuote) {
        ArrayList<LObject> data = new ArrayList<>();  
        while (true) {
            try {
                while (getCurrentChar() != '[') {
                    if ((getCurrentChar() == ' ') || (getCurrentChar() == '\n') || (getCurrentChar() == '\t')) {
                        advance();
                        continue;
                    }
                    data.add(parseObject(isBackQuote));
                }
                break;
            } catch (LineBeginningException e) {
                if (countObservers() == 0)
                    throw new MissingClosingBracketException();
                setChanged();
                notifyObservers(new MissingClosingBracketException());
                clearChanged();
            }
        }
        advanceTo(getMyCurrentIndex() - 1);
        return new LispVector(Lists.reverse(data));
    }

    private int getNextDoubleQuoteIndex (int from) {
        int i = myLispCode.lastIndexOf('"', from);
        if (i == -1)
            return i;
        if (i != 0 && myLispCode.charAt(i - 1) == '\\')
            i = getNextDoubleQuoteIndex(i - 1);
        return i;
    }

    private int getNextIndexOf (char what) {
        return (what == '"') ? getNextDoubleQuoteIndex(getMyCurrentIndex()) : myLispCode.lastIndexOf(what, getMyCurrentIndex());
    }

    private LispString parseString() {
        int nextDoubleQuoteIndex;
        while (true) {
            nextDoubleQuoteIndex = getNextIndexOf('"');
            if (nextDoubleQuoteIndex == -1) {
                if (countObservers() == 0)
                    throw new MissingClosingDoubleQuoteException();
                setChanged();
                notifyObservers(new MissingClosingDoubleQuoteException());
                clearChanged();
                continue;
            }
            break;
        }
        String string = myLispCode.substring(nextDoubleQuoteIndex+1, getMyCurrentIndex()+1);
        advanceTo(nextDoubleQuoteIndex - 1);
        return new LispString(string);
    }

    private int getNextSeparatorIndex()  {
        ArrayList<Integer> nextSeparatorIndex = new ArrayList<Integer>();
        for (char separator : mySeparators) {
            nextSeparatorIndex.add(getNextIndexOf(separator));
        }
        return Collections.max(nextSeparatorIndex);
    }

    private LispObject parseNumber ()  {
        int nextSeparatorIndex = getNextSeparatorIndex();
        String numberCandidate = myLispCode.substring(nextSeparatorIndex+1, getMyCurrentIndex()+1);
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

    private LispObject parseSymbol () {
        int nextSeparatorIndex = getNextSeparatorIndex();
        int currentIndex = getMyCurrentIndex();
        String symbol = myLispCode.substring(nextSeparatorIndex+1, currentIndex+1);
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

    private LispInteger parseCharacter () {
        int nextSeparatorIndex = getNextSeparatorIndex();
        int currentIndex = getMyCurrentIndex();
        String character = myLispCode.substring(nextSeparatorIndex, currentIndex);
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
        } catch (LineBeginningException ignored) {
            return lispObject;
        }
        throw new UnknownCodeBlockException(myLispCode.substring(0, getMyCurrentIndex()));
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
        } catch (LineBeginningException e) {
            return LispSymbol.ourNil;
        }

        switch (getCurrentChar()) {
            case '\'':
                advance();
                return parseQuote(isBackQuote);
            case '"':
                advance();
                return parseString();
            case ')':
                advance();
                return parseList(isBackQuote);
            case ']':
                advance();
                return parseVector(isBackQuote);
            case '?':
                advance();
                return parseCharacter();
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
                throw new UnknownCodeBlockException(myLispCode.substring(getNextIndexOf('\n'), myCurrentIndex));
        }

        return lispObject;
    }

    public void append (String lispCode) {
        myLispCode += lispCode;
    }

}

