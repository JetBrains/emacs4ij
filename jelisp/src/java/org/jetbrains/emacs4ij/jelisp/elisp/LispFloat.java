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
 */
public class LispFloat extends LispNumber{

    private double myData;

    public LispFloat(double myData) {
        this.myData = myData;
    }

    //TODO: To test whether a floating point value is a NaN, compare it with itself using =.
    // That returns nil for a NaN, and t for any other floating point value.

    //TODO: NAN and so on

    @Override
    public String toString() {
        return "LispFloat{" +
                "myData=" + myData +
                '}';
    }

    @Override
    public LispString toLispString() {
        return new LispString(Double.toString(myData));
    }
}
