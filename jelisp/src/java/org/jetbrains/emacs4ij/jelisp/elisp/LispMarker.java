package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/3/11
 * Time: 4:03 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispMarker extends LispObject {
    private int myPosition;
    private LispBuffer myBuffer;
    private LispSymbol myInsertionType; // t = after, nil = before inserted text

    public LispMarker () {
        myPosition = 0;
        myBuffer = null;
        myInsertionType = LispSymbol.ourNil;
    }

    public LispMarker (int position, LispBuffer buffer) {
        myPosition = position;
        myBuffer = buffer;
        myInsertionType = LispSymbol.ourNil;
    }

    public LispMarker (LispMarker marker) {
        myPosition = marker.myPosition;
        myBuffer = marker.myBuffer;
        myInsertionType = marker.myInsertionType;
    }

    public LispSymbol getInsertionType() {
        return myInsertionType;
    }

    public LObject setInsertionType (LObject type) {
        if (type.equals(LispSymbol.ourNil))
            myInsertionType = LispSymbol.ourNil;
        else
            myInsertionType = LispSymbol.ourT;
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispMarker marker = (LispMarker) o;

        if (myPosition != marker.myPosition) return false;
        if (myBuffer != null ? !myBuffer.equals(marker.myBuffer) : marker.myBuffer != null) return false;
        if (myInsertionType != null ? !myInsertionType.equals(marker.myInsertionType) : marker.myInsertionType != null)
            return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myPosition;
        result = 31 * result + (myBuffer != null ? myBuffer.hashCode() : 0);
        result = 31 * result + (myInsertionType != null ? myInsertionType.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        if (myBuffer == null)
            return "#<marker in no buffer>";
        return "#<marker at " + myPosition + " in " + myBuffer.getName() + '>';
    }

    public int getPosition() {
        return myPosition;
    }

    public void setPosition(int myPosition) {
        this.myPosition = myPosition;
    }

    @Override
    public LObject evaluate(Environment environment) {
        return this;
    }
}
