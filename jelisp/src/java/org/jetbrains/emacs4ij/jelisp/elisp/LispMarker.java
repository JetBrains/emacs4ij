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
        if (buffer != null) buffer.addMarker(this);
    }

    public LispMarker (LObject position, LispBuffer buffer) {
        myBuffer = buffer;
        setPosition(position);
        myInsertionType = LispSymbol.ourNil;
        if (buffer != null) buffer.addMarker(this);
    }

    public LispMarker (LispMarker marker) {
        myBuffer = marker.myBuffer;
        myPosition = marker.myPosition;
        myInsertionType = marker.myInsertionType;
        if (myBuffer != null) myBuffer.addMarker(this);
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
        if (!(o instanceof LispMarker)) return false;

        LispMarker marker = (LispMarker) o;

        if (myBuffer != null ? !myBuffer.equals(marker.myBuffer) : marker.myBuffer != null) return false;
        if (myInsertionType != null ? !myInsertionType.equals(marker.myInsertionType) : marker.myInsertionType != null)
            return false;
        if (myPosition != null ? !myPosition.equals(marker.myPosition) : marker.myPosition != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myPosition != null ? myPosition.hashCode() : 0;
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

    public LispBuffer getBuffer() {
        return myBuffer;
    }

    public void setPosition(LObject position) {
        if (position.equals(LispSymbol.ourNil)) {
            myPosition = position;
            return;
        }
        if (position instanceof LispMarker) {
            setPosition(((LispMarker) position).getPosition());
            return;
        }
        if (position instanceof LispInteger) {
            setPosition(((LispInteger) position).getData());
            return;
        }
        throw new WrongTypeArgumentException("integer-or-nil", position.toString());
    }

    public void setBuffer (LispBuffer buffer) {
        if (myBuffer != buffer) {
            if (myBuffer != null)
                myBuffer.removeMarker(this);
            if (buffer != null)
                buffer.addMarker(this);
        }
        myBuffer = buffer;
    }

    public void setPosition(int position) {
        int p = position;
        if (p < 1)
            p = 1;
        if (myBuffer != null)
            if (p > myBuffer.pointMax())
                p = myBuffer.pointMax();
        myPosition = new LispInteger(p);
    }

    @Override
    public LObject evaluate(Environment environment) {
        return this;
    }
}
