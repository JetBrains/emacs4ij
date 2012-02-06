package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/3/11
 * Time: 4:03 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispMarker extends LispObject {
    private LObject myPosition;
    private LispBuffer myBuffer;
    private LispSymbol myInsertionType; // t = after, nil = before inserted text

    public LispMarker () {
        myPosition = LispSymbol.ourNil;
        myBuffer = null;
        myInsertionType = LispSymbol.ourNil;
    }

    public LispMarker (int position, LispBuffer buffer) {
        myBuffer = buffer;
        setPosition(position);
        myInsertionType = LispSymbol.ourNil;
    }

    public LispMarker (LObject position, LispBuffer buffer) {
        myBuffer = buffer;
        setPosition(position);
        myInsertionType = LispSymbol.ourNil;
    }

    public LispMarker (LispMarker marker) {
        myBuffer = marker.myBuffer;
        myPosition = marker.myPosition;
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
        int result = myPosition.hashCode();
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

    public LObject getPosition() {
        return myPosition;
    }

    public void setPosition(LObject position) {
        if (position.equals(LispSymbol.ourNil)) {
            myPosition = position;
            return;
        }
        if (position instanceof LispInteger) {
            setPosition(((LispInteger) position).getData());
            return;
        }
        throw new WrongTypeArgumentException("integer-or-nil", position.toString());
    }

    public void setBuffer (LispBuffer buffer) {
        myBuffer = buffer;
    }

    public void setPosition(int position) {
        int p = position;
        if (p < 1)
            p = 1;
        if (p > myBuffer.pointMax())
            p = myBuffer.pointMax();
        myPosition = new LispInteger(p);
    }

    @Override
    public LObject evaluate(Environment environment) {
        return this;
    }
}
