package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.ArgumentOutOfRange;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.*;
import java.util.regex.Matcher;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 26/02/12
 * Time: 18:06
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsString {
    private BuiltinsString () {}

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
                    throw new LispException("Invalid format operation %" + c);
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
                data[i] = object instanceof LispNumber ? ((LispNumber)object).getData() : object.toString();
            }
            return new LispString(String.format(formatString.getData(), data));
        } catch (MissingFormatArgumentException e1) {
            throw new LispException("Not enough arguments for format string");
        } catch (UnknownFormatConversionException e2) {
            char c = e2.getMessage().charAt(e2.getMessage().length() - 2);
            if (c < 'A')
                throw new LispException("Format string ends in middle of format specifier");
            throw new LispException("Invalid format operation %" + c);
        } catch (IllegalFormatConversionException e3) {
            throw new LispException("Format specifier doesn't match argument type");
        }
    }

    @Subroutine(value = "string-match")
    public static LispObject stringMatch (Environment environment, LispString regexp, LispString string, @Optional LispInteger start) {
        int from = 0;
        if (start != null) {
            from = start.getData();
            if (from < 0 || from >= string.length())
                throw new ArgumentOutOfRange(string.toString(), start.toString());
        }
        LispSymbol s = environment.find("case-fold-search");
        int r = string.match(regexp, from, (s != null && !s.getValue().equals(LispSymbol.ourNil)));
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
    public static LispObject capitalize (LispObject object) {
        if (object instanceof LispString) {
            return ((LispString)object).capitalize();
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

    @Subroutine("match-beginning")
    public static LispObject matchBeginning (LispInteger subExp) {
        int index = subExp.getData();
        if (index < 0)
            throw new ArgumentOutOfRange(subExp, 0);
        Matcher m = GlobalEnvironment.INSTANCE.getLastSearchResult();
        if (m == null)
            return LispSymbol.ourNil;
        if (index == 0)
            return new LispInteger(m.start());
        if (index > m.groupCount())
            return LispSymbol.ourNil;
        return new LispInteger(m.start(index));
    }

    @Subroutine("match-end")
    public static LispObject matchEnd (LispInteger subExp) {
        int index = subExp.getData();
        if (index < 0)
            throw new ArgumentOutOfRange(subExp, 0);
        Matcher m = GlobalEnvironment.INSTANCE.getLastSearchResult();
        if (m == null)
            return LispSymbol.ourNil;
        if (index == 0)
            return new LispInteger(m.end());
        if (index > m.groupCount())
            return LispSymbol.ourNil;
        return new LispInteger(m.end(index));
    }

    private static int getIntegerData (LispObject object) {
        if (object instanceof LispInteger)
            return ((LispInteger) object).getData();
        throw new WrongTypeArgumentException("integerp", object.toString());
    }

    @Subroutine("string-to-number")
    public static LispNumber stringToNumber (LispString string, @Optional LispObject baseObject) {
        int base = BuiltinPredicates.isNil(baseObject) ? 10 : getIntegerData(baseObject);
        return string.toNumber(base);
    }
}
