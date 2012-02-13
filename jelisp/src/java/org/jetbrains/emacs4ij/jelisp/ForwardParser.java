package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.*;

import java.util.ArrayList;
import java.util.Collections;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 12:59 PM
 *
 * this is a parser for lisp program
 */

public class ForwardParser extends Parser {

    @Override 
    protected void advance() {
        if (myCurrentIndex == myLispCode.length())
            throw new EndOfLineException();
        ++myCurrentIndex;
    }

    @Override
    protected int getMyCurrentIndex() {
        if (myCurrentIndex == myLispCode.length())
            throw new EndOfLineException();
        return myCurrentIndex;
    }

    @Override
    protected char getNextChar() {
        return myLispCode.charAt(getMyCurrentIndex() + 1);
    }

    @Override
    protected boolean hasNextChar() {
        return myCurrentIndex < myLispCode.length() - 1;
    }

    private void skipListSeparators() {
        while (true) {
            if (myInnerSeparators.contains(getCurrentChar())) {
                advance();
                continue;
            }
            break;
        }
    }

    private LObject parseList(boolean isBackQuote) {
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
                        if (myInnerSeparators.contains(getNextChar())) {
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
        if (makeList && (!(data.size() == 1 && wasCons)))
            return LispList.list(data);
            
        return data.get(0);
    }

    private LispObject parseVector(boolean isBackQuote) {
        LispVector vector = new LispVector();
        while (true) {
            try {
                while (true) {
                    skipListSeparators();
                    if (getCurrentChar() == ']')
                        break;
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
                    i = getNextDoubleQuoteIndex(i + 1);
                } else {
                    return -1;
                }
        return i;
    }

    @Override
    protected int getNextIndexOf (char what) {
        int i = (what == '"') ? getNextDoubleQuoteIndex(getMyCurrentIndex()) : myLispCode.indexOf(what, getMyCurrentIndex());
        return ((i == -1) ? myLispCode.length() : i);
    }

    @Override
    protected String extractForm(int nextSeparatorIndex) {
        return myLispCode.substring(getMyCurrentIndex(), nextSeparatorIndex);
    }

    private LispString parseString() {
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
        String string = extractForm(nextDoubleQuoteIndex);
        advanceTo(nextDoubleQuoteIndex + 1);
        return new LispString(string);
    }

    @Override
    protected int getNextSeparatorIndex() {
        ArrayList<Integer> nextSeparatorIndex = new ArrayList<Integer>();
        for (char separator : mySeparators) {
            nextSeparatorIndex.add(getNextIndexOf(separator));
        }
        return Collections.min(nextSeparatorIndex);
    }

    private LispInteger parseCharacter () {
        int nextSeparatorIndex = getNextSeparatorIndex();
        String character = extractForm(nextSeparatorIndex);
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

    @Override
    public LObject parseLine (String lispCode) {
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

    @Override
    protected LObject tryToParse(boolean isBackQuote) {
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
                    if (myInnerSeparators.contains(getNextChar()))
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

    @Override
    public void append (String lispCode) {
        myLispCode += "\n" + lispCode;
    }

}