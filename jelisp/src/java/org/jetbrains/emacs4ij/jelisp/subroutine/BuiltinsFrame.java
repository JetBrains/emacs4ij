package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 7:14 PM
 * To change this template use File | Settings | File Templates.
 */
public class BuiltinsFrame {
    @Subroutine("selected-frame")
    public static LObject selectedFrame () {
        return GlobalEnvironment.getSelectedFrame() == null ?
                LispSymbol.ourNil : GlobalEnvironment.getSelectedFrame();
    }

    @Subroutine("frame-parameter")
    public static LObject frameParameter (LObject frame, LispSymbol parameter) {
        System.out.println("Ask for frame parameter: " + parameter.getName());
        if (frame.equals(LispSymbol.ourNil)) {
            frame = GlobalEnvironment.getSelectedFrame();
        }
        if (frame == null)
            return LispSymbol.ourNil;
        if (!(frame instanceof LispFrame))
            throw new WrongTypeArgumentException("framep", frame.toString());
        return ((LispFrame) frame).getParameter(parameter.getName());
    }
}
