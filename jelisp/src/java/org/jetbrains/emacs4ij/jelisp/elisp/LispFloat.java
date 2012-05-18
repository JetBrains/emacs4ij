package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/12/11
 * Time: 4:46 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp float number = ‘1500.0’, ‘15e2’, ‘15.0e2’, ‘1.5e3’
 * it is the same as the range of the C data type double on the machine you are using
 *
 * TODO: To test whether a floating point value is a NaN, compare it with itself using =.
 * That returns nil for a NaN, and t for any other floating point value.
 */
public class LispFloat extends LispNumber<Double> {
    public static final LispFloat ourPositiveInfinity = new LispFloat(Double.POSITIVE_INFINITY);
    public static final LispFloat ourNegativeInfinity = new LispFloat(Double.NEGATIVE_INFINITY);
    public static final LispFloat ourNaN = new LispFloat(Double.NaN);

    public LispFloat(double data) {
        myData = data;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
            //return this != ourNaN;
        }
        if (o == null || getClass() != o.getClass()) return false;
        LispFloat lispFloat = (LispFloat) o;
        return Double.compare(lispFloat.myData, myData) == 0;
    }

    @Override
    public int hashCode() {
        long temp = myData != +0.0d ? Double.doubleToLongBits(myData) : 0L;
        return (int) (temp ^ (temp >>> 32));
    }
}
