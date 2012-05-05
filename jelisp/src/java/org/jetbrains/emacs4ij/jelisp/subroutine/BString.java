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
            if (from < 0 || from >= string.length())
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
        //todo: write in echo area
        LispString s = format(formatString, args);
        System.out.println(s.getData());
        return s;
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
        int end = getInt(finish, string.length());
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
}
