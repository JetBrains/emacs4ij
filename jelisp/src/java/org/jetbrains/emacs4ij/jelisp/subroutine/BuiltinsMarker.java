package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/3/11
 * Time: 4:38 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsMarker {
    @Subroutine("make-marker")
    public static LispMarker makeMarker () {
        return new LispMarker();
    }

    @Subroutine("point-marker")
    public static LispMarker pointMarker (Environment environment) {
        LispBuffer buffer = environment.getBufferCurrentForEditing();
        return new LispMarker(buffer.point(), buffer);
    }

    @Subroutine ("marker-insertion-type")
    public static LispSymbol markerInsertionType (LispMarker marker) {
        return marker.getInsertionType();
    }

    @Subroutine("set-marker-insertion-type")
    public static LispObject setMarkerInsertionType (LispMarker marker, LispObject type) {
        return marker.setInsertionType(type);
    }

    @Subroutine("point-min-marker")
    public static LispMarker pointMinMarker (Environment environment) {
        LispBuffer buffer = environment.getBufferCurrentForEditing();
        return new LispMarker(buffer.pointMin(), buffer);
    }

    @Subroutine("point-max-marker")
    public static LispMarker pointMaxMarker (Environment environment) {
        LispBuffer buffer = environment.getBufferCurrentForEditing();
        return new LispMarker(buffer.pointMax(), buffer);
    }

    @Subroutine("copy-marker")
    public static LispMarker copyMarker (Environment environment, MarkerOrInteger markerOrInteger, @Optional LispObject insertionType) {
        LispMarker marker = new LispMarker();
        marker.set(markerOrInteger.getPosition(), markerOrInteger.getBuffer(environment));
        if (insertionType != null)
            marker.setInsertionType(insertionType);
        return marker;
    }

    @Subroutine("set-marker")
    public static LispMarker setMarker (LispMarker marker, MarkerOrInteger markerOrInteger, @Optional LispBuffer buffer) {
        if (BuiltinPredicates.isNil(buffer))
            buffer = GlobalEnvironment.INSTANCE.getBufferCurrentForEditing();
        marker.set(markerOrInteger.getPosition(), buffer);
        return marker;
    }

    @Subroutine("move-marker")
    public static LispMarker moveMarker (LispMarker marker, MarkerOrInteger markerOrInteger, @Optional LispBuffer buffer) {
        return setMarker(marker, markerOrInteger, buffer);
    }
    
    @Subroutine("marker-position")
    public static LispObject markerPosition (LispMarker marker) {
        return marker.getPosition() == null ? LispSymbol.ourNil : new LispInteger(marker.getPosition());
    }

    @Subroutine("marker-buffer")
    public static LispObject markerBuffer (LispMarker marker) {
        return BuiltinsCore.thisOrNil(marker.getBuffer());
    }
    
    @Subroutine("buffer-has-markers-at")
    public static LispSymbol bufferHasMarkersAt (Environment environment, LispObject position) {
        if (position instanceof LispInteger) 
            return LispSymbol.bool(environment.getBufferCurrentForEditing().hasMarkersAt(((LispInteger) position).getData()));
        return LispSymbol.ourNil;
    }

    @Subroutine("mark-marker")
    public static LispMarker markMarker (Environment environment) {
        return environment.getBufferCurrentForEditing().getMark();
    }
    
    @Subroutine("region-beginning")
    public static LispInteger regionBeginning (Environment environment) {
        LispObject mark = LispList.list(new LispSymbol("mark"), LispSymbol.ourT).evaluate(environment);
        LispInteger point = new LispInteger(environment.getBufferCurrentForEditing().point());
        if (!mark.equals(LispSymbol.ourNil)) {
            if (BuiltinArithmetic.less(mark, point).toBoolean())
                return (LispInteger)mark;
        }
        return point;
    }

    @Subroutine("region-end")
    public static LispInteger regionEnd (Environment environment) {
        LispObject mark = LispList.list(new LispSymbol("mark"), LispSymbol.ourT).evaluate(environment);
        LispInteger point = new LispInteger(environment.getBufferCurrentForEditing().point());
        if (!mark.equals(LispSymbol.ourNil)) {
            if (BuiltinArithmetic.more(mark, point).toBoolean())
                return (LispInteger)mark;
        }
        return point;
    }
}
