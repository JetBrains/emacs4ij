package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.Nullable;
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

    private LispObject parseList(boolean isBackQuote) {
        ArrayList<LispObject> data = new ArrayList<>();
        boolean makeList = true;
        boolean wasCons = false;
        while (true) {
            try {
                while (true) {
                    skipListSeparators();
                    if (getCurrentChar() == ')')
                        break;
                    if (!makeList)
                        throw new InvalidReadSyntax(JelispBundle.message("dot.in.wrong.context"));
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
                                LispObject object = parseObject(isBackQuote);
                                if (object != null)
                                    data.add(object);
                                continue;
                            }
                            LispObject car = data.get(data.size()-1);
                            LispObject cdr = parseObject(isBackQuote);
                            while (cdr == null) {
                                advance();
                                if (getCurrentChar() == ')')
                                    throw new InvalidReadSyntax(")");
                                cdr = parseObject(isBackQuote);
                            }
                            data.set(data.size()-1, LispList.cons(car, cdr));
                            wasCons = true;
                            skipListSeparators();
                            if (getCurrentChar() != ')')
                                throw new InvalidReadSyntax(JelispBundle.message("dot.in.wrong.context"));
                            break;
                        }
                    }
                    LispObject object = parseObject(isBackQuote);
                    if (object != null) {
                        data.add(object);
                    }
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
        if (makeList && (!(data.size() == 1 && wasCons))) {
            if (wasCons)
                return LispList.listAsIs(data);
            return LispList.list(data);
        }
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
            return myLispCode.length();
        if (i != 0)
            if (myLispCode.charAt(i-1) == '\\')
                if (i + 1 != myLispCode.length()) {
                    i = getNextDoubleQuoteIndex(i + 1);
                } else {
                    return myLispCode.length();
                }
        return i;
    }

    @Override
    protected int getNextIndexOf (char what) {
        if (myCurrentIndex == myLispCode.length())
            return myLispCode.length();
        int i = what == '"' ? getNextDoubleQuoteIndex(getMyCurrentIndex()) : myLispCode.indexOf(what, getMyCurrentIndex());
        return i == -1 ? myLispCode.length() : i;
    }

    @Override
    protected String extractForm(int nextSeparatorIndex) {
        return myCurrentIndex < 0 || myCurrentIndex >= myLispCode.length()
                ? ""
                : myLispCode.substring(getMyCurrentIndex(), nextSeparatorIndex);
    }

    private LispString parseString() {
        int nextDoubleQuoteIndex;
        String data = "";
        while (true) {
            nextDoubleQuoteIndex = getNextDoubleQuoteIndex(myCurrentIndex);
            data += extractForm(nextDoubleQuoteIndex);
            if (nextDoubleQuoteIndex == myLispCode.length()) {
                if (countObservers() == 0)
                    throw new MissingClosingDoubleQuoteException();
                setChanged();
                notifyObservers(new MissingClosingDoubleQuoteException());
                clearChanged();
                data += '\n';
                continue;
            }
            break;
        }
        advanceTo(nextDoubleQuoteIndex + 1);
        return new LispString(data);
    }

    @Override
    protected int getNextSeparatorIndex() {
        ArrayList<Integer> nextSeparatorIndex = new ArrayList<>();
        for (char separator : mySeparators) {
            nextSeparatorIndex.add(getNextIndexOf(separator));
        }
        return Collections.min(nextSeparatorIndex);
    }

    private static int convert(char c) {
        char ch = (Character.toLowerCase(c) == 'a') ? c : Character.toLowerCase(c);
        switch (ch) {
            case 'a': case '7':  //control-g, C-g  == Quit
                return 7;
            case 'b': case '\b'://backspace, <BS>, C-h
                return '\b';
            case 't': case '\t': //tab, <TAB>, C-i
                return '\t';
            case 'n': case '\n': //newline, C-j
                return '\n';
            case 'v': case 11: //vertical tab, C-k
                return 11;
            case 'f': case '\f': //formfeed character, C-l
                return '\f';
            case 'r': case '\r': //carriage return, <RET>, C-m
                return '\r';
            case 'e': case 27: //escape character, <ESC>, C-[
                return 27;
//            case 's': //space character, <SPC>
//                return ' ';
            case '\\': //backslash character, \
                return '\\';
            case 'd': case 127: //delete character, <DEL>
                return 127;
            default:
                return -1;
        }
    }

    private static int ctrlConvert (char c, boolean asIs) {
        switch (Character.toLowerCase(c)) {
            case 'g':
                return convert('a');
            case 'h':
                return convert('b');
            case 'i':
                return convert('t');
            case 'j':
                return convert('n');
            case 'k':
                return convert('v');
            case 'l':
                return convert('f');
            case 'm':
                return convert('r');
            case '[':
                return convert('e');
            default:
                if (c <= '9' && c >= '0')
                    return c;
                int a = Character.toUpperCase(c) - 64;
                if (a >= 0 && a < 128)
                    return a;
                if (asIs || c < 33)
                    return c;
                if (c > 32 && a < 0)
                    throw new ScanException(JelispBundle.message("invaild.modifier.in.string"));
                return -1;
        }
    }

    private static class Char {
        public enum Modifier {M, C, S, H, s, A;
            public static int indexOf (Modifier m) {
                return Arrays.asList(values()).indexOf(m);
            }
        }
        private char myModifiers[] = new char[] {'0', '0', '0', '0', '0', '0'}; //MCSHsA
        private int myKey = -1;
        private int myCtrlCount = 0;
        private boolean isAsIs = false;
        public Char() {}

        public void setModifier(Modifier modifier) {
            myModifiers[Modifier.indexOf(modifier)] = '1';
            if (modifier == Modifier.C)
                myCtrlCount++;
        }

        public void setKey(int myKey, boolean asIs) {
            this.myKey = myKey;
            isAsIs = asIs;
        }

        public Integer toInteger() {
            if (myCtrlCount != 0) {
                int k = ctrlConvert((char)myKey, isAsIs);
                if (k < 0)
                    return null;
                if (myKey != k) {
                    myModifiers[1] = myCtrlCount == 1 ? '0' : '1';
                    myKey = k;
                }
            }
            String number = new String(myModifiers);
            String key = Integer.toBinaryString(myKey);
            for (int i = 6; i != 27 - key.length() + 1; ++i)
                number += '0';
            return Integer.valueOf(number.concat(key), 2);
        }
    }

    private void setCharKey (Char c, @Nullable Integer key, boolean asIs) {
        if (myLispCode.length() != myCurrentIndex + 1 && !mySeparators.contains(getNextChar()))// && getNextChar() != ']')
            throw new InvalidReadSyntax("?");
//        int ch = key == null ? Character.toLowerCase(getCurrentChar()) : key; //todo: not in ascii?
        int ch = key == null ? getCurrentChar() : key;
        c.setKey(ch, asIs);
        advanceTo(myCurrentIndex + 1);
    }

    private void setCharKey (Char c, @Nullable Integer key) {
        setCharKey(c, key, false);
    }

    private LispObject parseCharacter () {
        Char c = new Char();
        while (true) {
            advance();
            if (getCurrentChar() != '\\' ) {
                setCharKey(c, null);
                break;
            }
            advance();
            int spec = convert(getCurrentChar());
            if (spec != -1) {
                setCharKey(c, spec, true);
                break;
            }

            int next = hasNextChar() ? getNextChar() : -1;

            Char.Modifier m;
            try {
                m = Char.Modifier.valueOf(Character.toString(getCurrentChar()));
                if (next != '-' && getCurrentChar() != 's')
                    throw new ScanException(JelispBundle.message("invalid.esc.char.syntax"));
            } catch (IllegalArgumentException e) {
                m = null;
            }

            if (m == null && getCurrentChar() == '^') {
                m = Char.Modifier.C;
                myCurrentIndex--;
            }
            if (m != null) {
                if (m == Char.Modifier.s && next != '-') {
                    setCharKey(c, (int)' ');
                    break;
                }
                c.setModifier(m);
                advance();
            } else if (getCurrentChar() == '^') {
                //skip                
            } else if (getCurrentChar() <= '7' && getCurrentChar() >= '0') { //octal, up to 3 digits
                int nextSeparatorIndex = getNextSeparatorIndex();
                String octal = extractForm(nextSeparatorIndex);
                if (octal.length() > 3)
                    throw new InvalidReadSyntax("?");
                try {
                    int n = Integer.valueOf(octal, 8);
                    advanceTo(nextSeparatorIndex-1);
                    setCharKey(c, n);
                    break;
                } catch (NumberFormatException e) {
                    throw new InvalidReadSyntax("?");
                }
            } else if (getCurrentChar() == 'x') {//hex
                advance();
                int nextSeparatorIndex = getNextSeparatorIndex();
                String hex = extractForm(nextSeparatorIndex);
                try {
                    int n = Integer.valueOf(hex, 16);
                    advanceTo(nextSeparatorIndex-1);
                    setCharKey(c, n);
                    break;
                } catch (NumberFormatException e) {
                    //todo: value = -1 when all symbols are ok but the string is too long
                    throw new InvalidReadSyntax("?");
                }
            }
            else {
                setCharKey(c, (int)Character.toLowerCase(getCurrentChar()), true);
                break;
            }
        }
        Integer n = c.toInteger();
        return (n == null ? LispSymbol.ourNil : new LispInteger(n));
    }

    @Override
    public LispObject parseLine (String lispCode) {
        myCurrentIndex = 0;
        myLispCode = lispCode;
        try {
            skipListSeparators();
        } catch (EndOfLineException e) {
            return null;
        }
        LispObject lispObject = parseObject();
        try {
            skipListSeparators();
            getMyCurrentIndex();
        } catch (EndOfLineException ignored) {
            return lispObject;
        }
        if (getCurrentChar() == ';')
            return lispObject;
        throw new UnknownCodeBlockException(myLispCode.substring(getMyCurrentIndex()));
    }

    @Override
    protected LispObject tryToParse(boolean isBackQuote) {
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
                return parseCharacter();
            case ';':
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
        if (lispObject == null) {
            lispObject = parseSymbol();
            if (lispObject == null)
                throw new ScanException(JelispBundle.message("unexpected.expression.end"));
        }
        return lispObject;
    }

    @Override
    public void append (String lispCode) {
        myLispCode = lispCode;
        myCurrentIndex = 0;
    }
}