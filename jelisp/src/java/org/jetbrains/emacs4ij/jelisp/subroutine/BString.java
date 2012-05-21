package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFormatOperationException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 26/02/12
 * Time: 18:06
 * To change this template use File | Settings | File Templates.
 */
public abstract class BString {
    private static LispString myCurrentMessage = null;

    private BString() {}

    private static void checkFormatCharacters (String format) {
        List<Character> emacsFormatChars = Arrays.asList('s', 'S', 'd', 'o', 'x', 'X', 'e', 'f', 'g', 'c');
        boolean wait = false;
        for (int i = 0; i < format.length(); ++i) {
            char c = format.charAt(i);
            if (!wait && c == '%') {
                wait = true;
                continue;
            }
            if (wait && Character.isLetter(c)) {
                if (!emacsFormatChars.contains(c))
                    throw new InvalidFormatOperationException(c);
                wait = false;
            }
        }
    }

    @Subroutine(value = "format")
    public static LispString format (LispString formatString, @Optional LispObject... objects) {
        checkFormatCharacters(formatString.getData());
        try {
            Object[] data = new Object[objects.length];
            for (int i = 0; i != objects.length; i++) {
                LispObject object = objects[i];
                data[i] = object instanceof LispNumber
                        ? ((LispNumber)object).getData()
                        : (object instanceof LispString ? ((LispString)object).getData() : object.toString());
            }
            return new LispString(String.format(formatString.getData(), data));
        } catch (MissingFormatArgumentException e1) {
            throw new LispException(JelispBundle.message("few.args.for.format.string"));
        } catch (UnknownFormatConversionException e2) {
            char c = e2.getMessage().charAt(e2.getMessage().length() - 2);
            if (c < 'A')
                throw new LispException(JelispBundle.message("unexpected.format.string.end"));
            throw new InvalidFormatOperationException(c);
        } catch (IllegalFormatConversionException e3) {
            throw new LispException(JelispBundle.message("format.arg.types.mismatch"));

        }
    }

    @Subroutine(value = "string-match")
    public static LispObject stringMatch (Environment environment, LispString regexp, LispString string,
                                          @Optional LispInteger start) {
        int from = 0;
        if (start != null) {
            from = start.getData();
            if (from < 0 || from >= string.size())
                throw new ArgumentOutOfRange(string.toString(), start.toString());
        }
        LispSymbol s = environment.find("case-fold-search");
        int r = string.match(environment, regexp, from, (s != null && !s.getValue().equals(LispSymbol.ourNil)));
        if (r == -1)
            return LispSymbol.ourNil;
        return new LispInteger(r);
    }

    @Subroutine("message")
    public static LispString message (LispString formatString, @Optional LispObject... args) {
        myCurrentMessage = format(formatString, args);
        System.out.println(myCurrentMessage.getData());
        //todo: write in echo area here
        return myCurrentMessage;
    }

    @Subroutine("current-message")
    public static LispObject currentMessage() {
        return Core.thisOrNil(myCurrentMessage);
    }

    @Subroutine("capitalize")
    public static LispObject capitalize (Environment environment, LispObject object) {
        if (object instanceof LispString) {
            return new LispString(((LispString)object).capitalize(environment));
        }
        if (object instanceof LispInteger) {
            int data = ((LispInteger)object).getData();
            if (data >= 'a' && data <= 'z') {
                return new LispInteger(data - 'a' + 'A');
            }
            return new LispInteger(data);
        }
        throw new WrongTypeArgumentException("char-or-string-p", object);
    }

    @Subroutine("string-to-number")
    public static LispNumber stringToNumber (LispString string, @Optional LispObject baseObject) {
        int base = getInt(baseObject, 10);
        return string.toNumber(base);
    }

    private static int getInt (@Nullable LispObject intOrNil, int defaultValue) {
        if (Predicate.isNil(intOrNil))
            return defaultValue;
        if (!(intOrNil instanceof LispInteger))
            throw new WrongTypeArgumentException("integerp", intOrNil);
        return ((LispInteger) intOrNil).getData();
    }

    @Subroutine("read-from-string")
    public static LispList readFromString (LispString string, @Optional @Nullable LispObject start,
                                             @Optional @Nullable LispObject finish) {
        int begin = getInt(start, 0);
        int end = getInt(finish, string.size());
        try {
            String code = string.getData().substring(begin, end);
            ForwardParser forwardParser = new ForwardParser();
            LispObject read = Core.thisOrNil(forwardParser.parseLine(code));
            return LispList.cons(read, new LispInteger(begin + forwardParser.getCurrentIndex()));
        } catch (StringIndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(string, begin, end);
        }
    }

    @Subroutine("regexp-quote")
    public static LispString regexpQuote (LispString target) {
        return target.getExactRegexp();
    }

    private static String getDataOrName (LispObject object) {
        if (object instanceof LispString)
            return ((LispString) object).getData();
        if (object instanceof LispSymbol)
            return ((LispSymbol) object).getName();
        throw new WrongTypeArgumentException("stringp", object);
    }

    @Subroutine("string-lessp")
    public static LispSymbol stringLessP (LispObject one, LispObject two) {
        String s1 = getDataOrName(one);
        String s2 = getDataOrName(two);
        return s1.compareTo(s2) < 0 ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("string-equal")
    public static LispSymbol stringEqual (LispObject one, LispObject two) {
        String s1 = getDataOrName(one);
        String s2 = getDataOrName(two);
        return LispSymbol.bool(s1.equals(s2));
    }

    private static LispInteger getInt (LispObject object) {
        if (!(object instanceof LispInteger))
            throw new WrongTypeArgumentException("integerp", object);
        return (LispInteger) object;
    }

    private static int processBound (LispInteger bound, int length) {
        return bound.getData() < 0 ? length + bound.getData() : bound.getData();
    }

    @Subroutine("substring")
    public static LispObject substring (StringOrVector stringOrVector, LispInteger from, @Optional LispObject to) {
        int length = stringOrVector.size();
        int start = processBound(from, length);
        int end = Predicate.isNil(to) ? length : processBound(getInt(to), length);
        try {
            return stringOrVector.substring(start, end);
        } catch (IndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(stringOrVector, start, end);
        }
    }

    @Subroutine("substring-no-properties")
    public static LispString substringNoProperties(LispString string, @Optional LispObject from, LispObject to) {
        int length = string.size();
        int start = Predicate.isNil(from) ? 0 : processBound(getInt(from), length);
        int end = Predicate.isNil(to) ? length : processBound(getInt(to), length);
        try {
            return string.substring(start, end, false);
        } catch (IndexOutOfBoundsException e) {
            throw new ArgumentOutOfRange(string, start, end);
        }
    }

    @Subroutine("char-to-string")
    public static LispString charToString (LispInteger character) {
        return new LispString(character.toCharacterString());
    }

    @Subroutine("number-to-string")
    public static LispString numberToString (LispNumber number) {
        return new LispString(number.toString());
    }

    @Subroutine("string-to-char")
    public static LispInteger stringToChar(LispString string) {
        return new LispInteger(string.getFirstCharacter());
    }

    @Subroutine("byte-to-string")
    public static LispString byteToString (LispObject byteObject) {
        throw new UnsupportedOperationException("byte-to-string");
    }
}
