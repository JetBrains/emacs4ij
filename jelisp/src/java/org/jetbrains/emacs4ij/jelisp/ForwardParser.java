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

    //todo: find appropriate chars
    private int convert(char c) {
        switch (c) {
            case 'a':  //control-g, C-g  == Quit
                return 7;
            case 'b': //backspace, <BS>, C-h
                return '\b';
            case 't': //tab, <TAB>, C-i
                return '\t';
            case 'n': //newline, C-j
                return '\n';
            case 'v': //vertical tab, C-k
                return 11;
            case 'f': //formfeed character, C-l
                return '\f';
            case 'r': //carriage return, <RET>, C-m
                return '\r';
            case 'e': //escape character, <ESC>, C-[
                return 27;
            case 's': //space character, <SPC>
                return ' ';
            case '\\': //backslash character, \
                return '\\';
            case 'd': //delete character, <DEL>
                return 127;
            default:
                return -1;
        }
    }
    
    private int ctrlConvert (char c) {
        switch (c) {
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
                return c;
        }
    }
    
    private void validate (String charSequence) {
        if (charSequence.length() != 3)
            throw new InvalidReadSyntax("?");
        if (charSequence.charAt(2) != '-' )
            throw new ScanException("Invalid escape character syntax");
    }

    private static class Char {
        public enum Modifier {M, C, S, H, s, A;
            public static int indexOf (Modifier m) {
                return Arrays.asList(values()).indexOf(m);
            }
        }
        private char myModifiers[] = new char[] {'0','0','0','0','0','0'}; //MCSHsA
        private int myKey = -1;
        public Char() {}

        public void setModifier(Modifier modifier) {
            myModifiers[Modifier.indexOf(modifier)] = '1';
        }

        public void setKey(int myKey) {
            this.myKey = myKey;
        }
        
        public int toInteger() {
            String number = new String(myModifiers);
            String key = Integer.toBinaryString(myKey);
            for (int i = 6; i != 27 - key.length(); ++i)
                number += '0';
            return Integer.valueOf(number.concat(key), 2);
        }
    }
    
    private void setCharKey (Char c, @Nullable Character key) {
        if (myLispCode.length() != myCurrentIndex + 1 && !mySeparators.contains(getNextChar()))
            throw new InvalidReadSyntax("?");
        int ch = key == null ? getCurrentChar() : key;
        c.setKey(ch);
        advanceTo(myCurrentIndex + 1);
    }

    private LispInteger parseCharacter () {
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
                setCharKey(c, (char)spec);
                break;
            }
            
            int next = hasNextChar() ? getNextChar() : '\n';
            
            if (next != '-' && getCurrentChar() != 's' && getCurrentChar() != '^')
                throw new ScanException("Invalid escape character syntax");
            
            Char.Modifier m = Char.Modifier.valueOf(Character.toString(getCurrentChar())); 
            if (m == null && getCurrentChar() == '^') {
                m = Char.Modifier.C;
                myCurrentIndex--;
            }
            if (m != null) {
                if (m == Char.Modifier.s && next != '-') {
                    setCharKey(c, ' ');
                    break;
                }
                c.setModifier(m);
                advance();                
            } else if (getCurrentChar() == '^') {
                
            } else {
                setCharKey(c, null);
                break;                
            }
        }
        return new LispInteger(c.toInteger());
    }
        
        
        /*if (character.charAt(0) == '\\') {
            if (character.length() == 1) {
                throw new ScanException("You must have forgotten the <space> between <?\\ > character and next code!");
            }
            switch (character.charAt(1)) {
                case '^': //Ctrl + following
                    answer = ctrlConvert(character.charAt(2));
                    break;
                case 'C':
                    validate(character);
                    answer = ctrlConvert(character.charAt(3));
                    break;
                case 'M':  //meta 2**27
                    validate(character);
                    throw new RuntimeException("I don't know " + character + " yet.");
                    break;
                case 'S': //shift 2**25
                    validate(character);
                    break;
                case 'H': //hyper 2**24
                    break;
                case 's': //super 2**23
                    if (character.charAt(2) == ' ') {
                        answer = ' ';
                        break;
                    }
                    validate(character);
                    break;
                case 'A': //alt 2**22
                    validate(character);
                    break;
                default:
                    answer = convert(character.charAt(1));
            }
        } else {
            answer = character.charAt(0);
        }
        advanceTo(nextSeparatorIndex);
        return new LispInteger(answer);
    }                             */

    @Override
    public LObject parseLine (String lispCode) {
        myCurrentIndex = 0;
        myLispCode = lispCode;
        //myLispCode = myLispCode.trim();
        LObject lispObject = parseObject();
        //if (lispObject == null) lispObject = LispSymbol.ourNil;
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
                //advance();
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