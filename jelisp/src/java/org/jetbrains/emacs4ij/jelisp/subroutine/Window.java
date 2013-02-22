package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/24/12
 * Time: 2:58 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Window {
    private Window () {}

    @NotNull
    private static LispWindow getWindow (Environment environment, LispObject object) {
        if (Predicate.isNil(object))
            return environment.getSelectedWindow();
        if (!(object instanceof LispWindow))
            throw new WrongTypeArgumentException("window-live-p", object);
        return (LispWindow) object;
    }

    private static boolean isMinibufferWindow (LispWindow window) {
        return window.getBuffer() instanceof LispMinibuffer;
    }

    @Subroutine("window-minibuffer-p")
    public static LispSymbol windowMinibufferP (Environment environment, @Optional LispObject window) {
        return LispSymbol.bool(isMinibufferWindow(getWindow(environment, window)));
    }

    @Subroutine("next-window")
    public static LispObject nextWindow(Environment environment,
                                        @Optional LispObject window, LispObject considerMinibuffer, LispObject allFrames) {
        window = getWindow(environment, window);
        if (considerMinibuffer == null)
            considerMinibuffer = LispSymbol.ourNil;
        if (allFrames == null) {
            allFrames = LispSymbol.ourNil;
        }

        List<LispFrame> frames = new ArrayList<>();
        if (allFrames.equals(LispSymbol.ourNil)) {
            LispFrame frame =  ((LispWindow) window).getFrame();
            frames.add(frame);
            boolean considerMinibuf = false;
            if ((considerMinibuffer.equals(LispSymbol.ourNil) && environment.getMiniBufferActivationsDepth() > 0)
                    || considerMinibuffer.equals(LispSymbol.ourT))
                considerMinibuf = true;
            if (considerMinibuf) {
                LispMinibuffer miniBuffer = environment.getFrameMinibuffer(frame);
                if (miniBuffer != null)
                    for (LispFrame otherFrame: environment.getBufferFrames(miniBuffer)) {
                        if (otherFrame != frame)
                            frames.add(otherFrame);
                    }
            }
        } else if (allFrames.equals(new LispSymbol("visible"))) {
            frames = environment.getVisibleFrames(); //search all visible frames
        } else if (allFrames.equals(LispSymbol.ourT)) { //search all frames.
            frames = environment.getAllFrames();
        } else if (allFrames.equals(new LispInteger(0))) { //search visible and iconified frames.
            frames = environment.getVisibleAndIconifiedFrames();
        } else if (allFrames instanceof LispFrame) { //search only that frame.
            frames.add((LispFrame) allFrames);
        } else {
            frames.add(((LispWindow) window).getFrame());
        }

        //make frames list
        List<LispWindow> windows = new ArrayList<>();
        for (LispFrame frame: frames) {
            windows.addAll(environment.getFrameWindows(frame));
        }
        int index = windows.indexOf(window);
        if (index == -1)
            throw new InternalException("next-window error");
        if (index >= windows.size() - 1)
            return windows.get(0);
        return windows.get(index + 1);
    }

    @Subroutine("window-buffer")
    public static LispBuffer windowBuffer (Environment environment, @Optional LispObject window) {
        return getWindow(environment, window).getBuffer();
    }

    @Subroutine(value = "delete-other-windows", isCmd = true, key = "\\C-x1")
    public static LispSymbol deleteOtherWindows (Environment environment, @Optional LispObject window) {
        window = getWindow(environment, window);
        if (isMinibufferWindow((LispWindow) window)) {
            throw new LispException(JelispBundle.message("minibuffer.window.cannot.fill.frame"));
        }
        environment.deleteFrameOtherWindows(((LispWindow) window).getFrame(), (LispWindow) window);
        return LispSymbol.ourNil;
    }

    @Subroutine("selected-window")
    public static LispWindow selectedWindow (Environment environment) {
        return environment.getSelectedWindow();
    }

    @Subroutine("window-start")
    public static LispObject windowStart (Environment environment, @Optional LispObject windowObject) {
        LispWindow window = getWindow(environment, windowObject);
        Integer start = window.getDisplayStart();
        return start == null ? LispSymbol.ourNil : new LispInteger(start);
    }

    @Subroutine("window-point")
    public static LispObject windowPoint (Environment environment, @Optional LispObject windowObject) {
        LispWindow window = getWindow(environment, windowObject);
        return new LispInteger(window.getBuffer().point());
    }

}
