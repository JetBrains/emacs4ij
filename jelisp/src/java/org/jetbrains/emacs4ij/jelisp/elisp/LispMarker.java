package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispBuffer;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/3/11
 * Time: 4:03 PM
 * To change this template use File | Settings | File Templates.
 */
public final class LispMarker implements MarkerOrInteger {
    private Integer myPosition;
    private LispBuffer myBuffer;
    private boolean isAfterInsertion; // true = after, false = before inserted text

    public LispMarker () {
        myPosition = null;
        myBuffer = null;
        isAfterInsertion = false;
    }

    public LispMarker (@Nullable Integer position, @NotNull LispBuffer buffer) {
        set(position, buffer);
        isAfterInsertion = false;
    }

    public LispSymbol getInsertionType() {
        return LispSymbol.bool(isAfterInsertion);
    }    

    public LispObject setInsertionType (LispObject type) {
        isAfterInsertion = !type.equals(LispSymbol.ourNil);
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LispMarker)) return false;

        LispMarker marker = (LispMarker) o;

        if (isAfterInsertion != marker.isAfterInsertion) return false;
        if (myBuffer != null ? !myBuffer.equals(marker.myBuffer) : marker.myBuffer != null) return false;
        if (myPosition != null ? !myPosition.equals(marker.myPosition) : marker.myPosition != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myPosition != null ? myPosition.hashCode() : 0;
        result = 31 * result + (myBuffer != null ? myBuffer.hashCode() : 0);
        result = 31 * result + (isAfterInsertion ? 1 : 0);
        return result;
    }

    @Override
    public String toString() {
        if (myBuffer == null)
            return "#<marker in no buffer>";
        return "#<marker at " + myPosition 
                + (isAfterInsertion ? " (moves after insertion) " : "") 
                + " in " + myBuffer.getName() + '>';
    }

    @Override
    public Integer getPosition() {
        return myPosition;
    }

    public LispBuffer getBuffer() {
        return myBuffer;
    }

    @Override
    public LispBuffer getBuffer(Environment environment) {
        return myBuffer;
    }

    public void set(@Nullable Integer position, @Nullable LispBuffer buffer) {
        if (position == null || buffer == null) {
            myPosition = null;
            if (myBuffer != null) myBuffer.removeMarker(this);
            myBuffer = null;
            return;
        }
        setBuffer(buffer);
        setPosition(position);
    }

    private void setPosition(int position) {
        int p = position;
        if (p < 1)
            p = 1;
        if (myBuffer != null)
            if (p > myBuffer.pointMax())
                p = myBuffer.pointMax();
        myPosition = p;
    }

    private void setBuffer (@Nullable LispBuffer buffer) {
        if (myBuffer != buffer) {
            if (myBuffer != null)
                myBuffer.removeMarker(this);
            if (buffer != null)
                buffer.addMarker(this);
        }
        myBuffer = buffer;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }
    
    public void insert (LispObject insertion) {
        if (myBuffer == null)
            throw new MarkerPointsNowhereException();
        myBuffer.insert(insertion, this);
    }
    
    public void move (int shift, int point, boolean moveAnyway) {
        if (myPosition == null || myPosition < point)
            return;
        if (shift < 0 || moveAnyway || (isAfterInsertion &&  point == myPosition) || point < myPosition) {
            setPosition(myPosition + shift);
        }
    }
    
    public boolean isSet () {
        return myBuffer != null && myPosition != null;
    }
}
