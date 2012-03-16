package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 7:14 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsFrame {
    private BuiltinsFrame() {}

    @Subroutine("selected-frame")
    public static LispObject selectedFrame () {
        return GlobalEnvironment.INSTANCE.getSelectedFrame() == null ?
                LispSymbol.ourNil : GlobalEnvironment.INSTANCE.getSelectedFrame();
    }

    @Subroutine("frame-parameter")
    public static LispObject frameParameter (LispObject frame, LispSymbol parameter) {
        System.out.println("Ask for frame parameter: " + parameter.getName());
        if (frame.equals(LispSymbol.ourNil)) {
            frame = GlobalEnvironment.INSTANCE.getSelectedFrame();
        }
        if (frame == null)
            return LispSymbol.ourNil;
        if (!(frame instanceof LispFrame))
            throw new WrongTypeArgumentException("framep", frame);
        return ((LispFrame) frame).getParameter(parameter.getName());
    }

    @Subroutine("get-buffer-window")
    public static LispObject getBufferWindow(Environment environment, @Optional LispObject bufferOrName, @Optional LispObject frame) {
        LispBuffer buffer = BuiltinsBuffer.getBufferByBufferNameOrNil(environment, bufferOrName);
        List<LispFrame> frames = new ArrayList<>();
        if (frame == null) {
            frame = LispSymbol.ourNil;
        }
        if (frame.equals(new LispSymbol("visible"))) {
            frames = GlobalEnvironment.getVisibleFrames(); //search all visible frames
        } else if (frame.equals(LispSymbol.ourT)) { //search all frames.
            frames = GlobalEnvironment.getAllFrames();
        } else if (frame.equals(new LispInteger(0))) { //search visible and iconified frames.
            frames = GlobalEnvironment.getVisibleAndIconifiedFrames();
        } else if (frame instanceof LispFrame) { //search only that frame.
            frames.add((LispFrame) frame);
        } else {
            LispObject selectedFrame = selectedFrame();
            if (!selectedFrame.equals(LispSymbol.ourNil))
                frames.add((LispFrame) selectedFrame);
        }

        for (LispFrame f: frames) {
            LispWindow window = f.containsBuffer(buffer);
            if (window != null)
                return window;
        }

        return LispSymbol.ourNil;
    }

    @Subroutine(value = "make-frame-visible", isCmd = true, interactive = "")
    public static LispObject makeFrameVisible(@Optional LispObject frame) {
        if (frame == null || frame.equals(LispSymbol.ourNil)) {
            frame = selectedFrame();
        }
        LispSymbol frameLiveP = BuiltinPredicates.frameLiveP(frame);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", frame);

        ((LispFrame)frame).setVisible(true);
        return frame;
    }

    @Subroutine(value = "make-frame-invisible", isCmd = true, interactive = "")
    public static LispObject makeFrameInvisible(@Optional LispObject frame, @Optional LispObject force) {
        if (frame == null || frame.equals(LispSymbol.ourNil)) {
            frame = selectedFrame();
        }
        LispSymbol frameLiveP = BuiltinPredicates.frameLiveP(frame);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", frame);

        if (force == null || force.equals(LispSymbol.ourNil)) {
            //check that exists one more visible frame than that we want to hide
            int k = ((LispFrame)frame).isVisible() ? 1 : 0;
            if (GlobalEnvironment.getVisibleAndIconifiedFrames().size() - k <= 0) {
                //todo: (error "Attempt to make invisible the sole visible or iconified frame")
                throw new RuntimeException("Attempt to make invisible the sole visible or iconified frame");
            }
        }

        ((LispFrame)frame).setVisible(false);

        return LispSymbol.ourNil;
    }

    @Subroutine(value = "iconify-frame", isCmd = true, interactive = "")
    public static LispObject iconifyFrame(@Optional LispObject frame) {
        if (frame == null || frame.equals(LispSymbol.ourNil)) {
            frame = selectedFrame();
        }
        LispSymbol frameLiveP = BuiltinPredicates.frameLiveP(frame);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", frame);

        ((LispFrame)frame).setIconified(true);

        return LispSymbol.ourNil;
    }
}
