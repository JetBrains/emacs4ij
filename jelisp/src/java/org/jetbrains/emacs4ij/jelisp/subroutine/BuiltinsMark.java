package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/21/11
 * Time: 6:00 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsMark {
     @Subroutine("mark-marker")
    public static LispMarker markMarker (Environment environment) {
        //todo:  Moving this marker changes the mark position
        return (LispMarker) environment.getBufferCurrentForEditing().getLocalVariableValue("my-mark");
    }

    /*
    //todo: it's a compiled elisp function

    @Subroutine("mark")
    public static LObject mark (Environment environment, @Optional LObject force) {


        Integer mark = environment.getBufferCurrentForEditing().getMark();
        return mark == null ? LispSymbol.ourNil : new LispInteger(mark);
    }



    //todo: it's a compiled elisp function
    @Subroutine("set-mark")
    public static LispMarker setMark (Environment environment, LispInteger position) {
        environment.getBufferCurrentForEditing().setMark(position.getData());
        return markMarker(environment);
    }

    //todo: it's a compiled elisp function
    @Subroutine("push-mark")
    public static LObject pushMark (Environment environment, @Optional LispInteger position, @Optional LObject noMsg, @Optional LObject activate) {
        Integer pos = position == null ? null : position.getData();
        boolean setActive = false;
        if (activate != null && activate.equals(LispSymbol.ourT))
            setActive = true;
        environment.getBufferCurrentForEditing().pushMark(pos, setActive);
        if (noMsg != null && !noMsg.equals(LispSymbol.ourNil))
            environment.getBufferCurrentForEditing().showMessage("Mark set.");
        return LispSymbol.ourNil;
    }

     //todo: it's a compiled elisp function
    @Subroutine("pop-mark")
    public static LObject popMark (Environment environment) {
        environment.getBufferCurrentForEditing().popMark();
        return LispSymbol.ourNil;
    }
    */

}
