package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;

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
    protected T myData;

    public T getData() {
        return myData;
    }

    public static <T> LispNumber newInstance (T data) {
        if (data instanceof Integer)
            return new LispInteger((Integer)data);
        if (data instanceof Double)
            return new LispFloat((Double)data);
        throw new LispException("Invalid LispNumber instantiation!");
    }

    /*public void setData(T myData) {
        this.myData = myData;
    }   */

    @Override
    /**
     * no parameters required
     */
    public LObject evaluate(Environment environment) {
        return this;
    }

}
