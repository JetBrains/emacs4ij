package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

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
        LispBuffer buffer = environment.getCurrentBuffer();
        return new LispMarker(buffer.point(), buffer);
    }

    @Subroutine ("marker-insertion-type")
    public static LispSymbol markerInsertionType (LispMarker marker) {
        return marker.getInsertionType();
    }

    @Subroutine("set-marker-insertion-type")
    public static LObject setMarkerInsertionType (LispMarker marker, LObject type) {
        return marker.setInsertionType(type);
    }

}
