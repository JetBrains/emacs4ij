package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 26/02/12
 * Time: 18:16
 * To change this template use File | Settings | File Templates.
 */
public abstract class Arithmetic {
    private Arithmetic() {}

    public static LispNumber numberOrMarkerToNumber (LispObject lispObject) {
        if (Predicate.numberOrMarkerP(lispObject).equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("number-or-marker-p", lispObject.toString());
        LispNumber n;
        if (Predicate.markerP(lispObject).equals(LispSymbol.ourT)) {
            if (((LispMarker)lispObject).isSet())
                n = new LispInteger(((LispMarker)lispObject).getPosition());
            else 
                throw new MarkerPointsNowhereException();
        } else {
            n = (LispNumber) lispObject;
        }
        return n;
    }

    private static LispNumber fromDouble (boolean isDouble, double d) {
        return isDouble ? new LispFloat(d) : new LispInteger((int) d);
    }

    @Subroutine("+")
    public static LispNumber plus (@Optional LispObject... args) {
        double ans = 0.0;
        boolean isDouble = false;
        if (args != null) {
            for (LispObject lispObject: args) {
                LispNumber n = numberOrMarkerToNumber(lispObject);
                if (!isDouble && (n.getData() instanceof Double))
                    isDouble = true;
                ans += n.getDoubleData();
            }
        }
        return fromDouble(isDouble, ans);
    }

    @Subroutine("-")
    public static LispNumber minus (@Optional LispObject num, @Optional LispObject... rest) {
        if (num == null)
            return new LispInteger(0);
        if (rest == null || rest.length == 0) {
            LispNumber n = numberOrMarkerToNumber(num);
            double ans = -n.getDoubleData();
            return fromDouble(n.getData() instanceof Double, ans);
        }
        LispNumber n = numberOrMarkerToNumber(num);
        double ans = n.getDoubleData();
        boolean isDouble = n.getData() instanceof Double;
        for (LispObject lispObject: rest) {
            LispNumber k = numberOrMarkerToNumber(lispObject);
            if (!isDouble && (k.getData() instanceof Double))
                isDouble = true;
            ans -= k.getDoubleData();
        }
        return fromDouble(isDouble, ans);
    }

    @Subroutine("*")
    public static LispNumber multiply (@Optional LispObject... args) {
        double ans = 1;
        boolean isDouble = false;
        for (LispObject lispObject: args) {
            LispNumber n = numberOrMarkerToNumber(lispObject);
            if (!isDouble && (n.getData() instanceof Double))
                isDouble = true;
            ans *= n.getDoubleData();
        }
        return fromDouble(isDouble, ans);
    }

    @Subroutine(">")
    public static LispSymbol more (LispObject num1, LispObject num2) {
        LispNumber n1 = numberOrMarkerToNumber(num1);
        LispNumber n2 = numberOrMarkerToNumber(num2);
        return LispSymbol.bool(n1.getDoubleData() > n2.getDoubleData());
    }

    @Subroutine("=")
    public static LispSymbol equalNumbersOrMarkers (LispObject num1, LispObject num2) {
        double n1 = numberOrMarkerToNumber(num1).getDoubleData();
        double n2 = numberOrMarkerToNumber(num2).getDoubleData();
        return LispSymbol.bool(n1 == n2);
    }

    @Subroutine("eql")
    public static LispSymbol eql (LispObject one, LispObject two) {
        if (one == two)
            return LispSymbol.ourT;
        if (one.getClass() != two.getClass())
            return LispSymbol.ourNil;
        if (!(one instanceof LispNumber))
            return LispSymbol.ourNil;
        return LispSymbol.bool(((LispNumber)one).getData().equals(((LispNumber)two).getData()));
    }

    @Subroutine("/=")
    public static LispSymbol notEqualNumbersOrMarkers (LispObject num1, LispObject num2) {
        double n1 = numberOrMarkerToNumber(num1).getDoubleData();
        double n2 = numberOrMarkerToNumber(num2).getDoubleData();
        return LispSymbol.bool(n1 != n2);
    }

    @Subroutine("<=")
    public static LispSymbol lessOrEqualNumbersOrMarkers (LispObject num1, LispObject num2) {
        double n1 = numberOrMarkerToNumber(num1).getDoubleData();
        double n2 = numberOrMarkerToNumber(num2).getDoubleData();
        return LispSymbol.bool(n1 <= n2);
    }

    @Subroutine("<")
    public static LispSymbol less (LispObject num1, LispObject num2) {
        double n1 = numberOrMarkerToNumber(num1).getDoubleData();
        double n2 = numberOrMarkerToNumber(num2).getDoubleData();
        return LispSymbol.bool(n1 < n2);
    }

    @Subroutine(">=")
    public static LispSymbol moreOrEqual (LispObject num1, LispObject num2) {
        return LispSymbol.bool(!less(num1, num2).toBoolean());
    }

    @Subroutine("1-")
    public static LispNumber minusOne (LispObject num) {
        LispNumber n = numberOrMarkerToNumber(num);
        boolean isDouble = n.getData() instanceof Double;
        return fromDouble(isDouble, n.getDoubleData() - 1);
    }

    @Subroutine("1+")
    public static LispNumber plusOne (LispObject num) {
        LispNumber n = numberOrMarkerToNumber(num);
        boolean isDouble = n.getData() instanceof Double;
        return fromDouble(isDouble, n.getDoubleData() + 1);
    }
    
    @Subroutine("logand")
    public static LispInteger logAnd (@Optional LispObject... args) {
        if (args == null)
            return new LispInteger(-1);
        int result = -1;
        for (LispObject arg: args) {
            LispNumber num = numberOrMarkerToNumber(arg);
            if (!(num instanceof LispInteger))
                throw new WrongTypeArgumentException("integer-or-marker-p", num.toString());
            result = result & ((LispInteger)num).getData();
        }
        return new LispInteger(result);
    }
    
    @Subroutine("lognot")
    public static LispInteger logNot (LispInteger object) {
        return new LispInteger(~object.getData());
    }

    @Subroutine("lsh")
    public static LispInteger leftShift (LispInteger value, LispInteger count) {
        return new LispInteger(value.getData() << count.getData());
    }

    @Subroutine("floor")
    public static LispInteger floor (LispNumber number, @Optional LispObject divisor) {
        if (!Predicate.isNil(divisor) && !(divisor instanceof LispNumber))
            throw new WrongTypeArgumentException("numberp", divisor);
        double d = Predicate.isNil(divisor) ? 1 : ((LispNumber)divisor).getDoubleData();
        double n = number.getDoubleData();
        return new LispInteger((int)Math.floor(n/d));
    }

    private static LispNumber makeNumber (double n) {
        return n == (int)n ? new LispInteger((int)n) : new LispFloat(n);
    }

    @Subroutine("/")
    public static LispNumber divide (LispObject dividend, LispObject divisor, @Optional LispObject... divisors) {
        double dividendN = numberOrMarkerToNumber(dividend).getDoubleData();
        double divisorN = numberOrMarkerToNumber(divisor).getDoubleData();
        double result = dividendN/divisorN;
        boolean isDouble = dividend instanceof LispFloat || divisor instanceof LispFloat;
        if (divisors != null)
            for (LispObject d: divisors) {
                result /= numberOrMarkerToNumber(d).getDoubleData();
                isDouble |= d instanceof LispFloat;
            }
        return fromDouble(isDouble, result);
    }

    @Subroutine("mod")
    public static LispNumber modulo (LispObject num1, LispObject num2) {
        double n1 = numberOrMarkerToNumber(num1).getDoubleData();
        double n2 = numberOrMarkerToNumber(num2).getDoubleData();
        return makeNumber(n1 % n2);
    }
}
