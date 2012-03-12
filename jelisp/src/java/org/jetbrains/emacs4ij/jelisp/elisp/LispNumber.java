package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:17 PM
 * To change this template use File | Settings | File Templates.
 *
 * base class for elisp numbers
 */
public abstract class LispNumber<T> extends LispAtom {
    public static int BITS_PER_EMACS_INT = Integer.SIZE;
    public static int GCTYPEBITS = 3;
    public static int VALBITS = BITS_PER_EMACS_INT - GCTYPEBITS;
    public static int MOST_NEGATIVE_FIXNUM = - 1 << (VALBITS - 1);
    public static int MOST_POSITIVE_FIXNUM = (1 << (VALBITS - 1)) - 1;
    public static int INTMASK = (1 << VALBITS) - 1;

    protected T myData;

    public T getData() {
        return myData;
    }

    public double getDoubleData() {
        return (this instanceof LispInteger) ? ((Integer)getData()).doubleValue() : (Double)getData();
    }

    @Override
    /**
     * no parameters required
     */
    public LObject evaluate(Environment environment) {
        return this;
    }

}
