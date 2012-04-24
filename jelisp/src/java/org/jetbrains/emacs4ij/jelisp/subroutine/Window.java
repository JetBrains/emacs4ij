package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.NoWindowException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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

    private static boolean isMinibufferWindow (Environment environment, LispWindow window) {
        return window == environment.getMinibufferWindow();
    }

    @Subroutine("window-minibuffer-p")
    public static LispSymbol windowMinibufferP (Environment environment, @Optional LispObject window) {
        return LispSymbol.bool(isMinibufferWindow(environment, getWindow(environment, window)));
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
            LispFrame frame = environment.getFrameByWindow((LispWindow) window);
            frames.add(frame);
            boolean considerMinibuf = false;
            if ((considerMinibuffer.equals(LispSymbol.ourNil) && environment.getMiniBufferActivationsDepth() > 0)
                    || considerMinibuffer.equals(LispSymbol.ourT))
                considerMinibuf = true;
            if (considerMinibuf) {
                LispMiniBuffer miniBuffer = frame.getMinibuffer();
                for (LispFrame otherFrame: environment.getFramesByBuffer(miniBuffer)) {
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
            frames.add(environment.getFrameByWindow((LispWindow) window));
        }

        //make frames list
        List<LispWindow> windows = new ArrayList<>();
        for (LispFrame frame: frames) {
            windows.addAll(frame.getWindows());
        }
        int index = windows.indexOf(window);
        if (index == -1)
            throw new InternalException("next-window error");
        if (index >= windows.size() - 1)
            return windows.get(0);
        return windows.get(index + 1);
    }

    private static LispFrame getFrame (Environment environment, LispWindow window) {
        LispFrame frame = environment.getFrameByWindow(window);
        if (frame == null)
            throw new InternalException(JelispBundle.message("no.window.frame", window.toString()));
        return frame;
    }

    private static LispBuffer getBufferByWindow (Environment environment, LispWindow window) {
        LispFrame frame = getFrame(environment, window);

        LispBuffer buffer = frame.getWindowBuffer(window);
        if (buffer == null)
            throw new InternalException(JelispBundle.message("no.buffer.frame.window", window.toString(), frame.toString()));

        return buffer;
    }

    @Subroutine("set-frame-selected-window")
    public static LispWindow setFrameSelectedWindow (Environment environment, LispObject frame,
                                                     LispWindow window, @Optional LispObject noRecord) {
        if (frame.equals(LispSymbol.ourNil))
            frame = environment.getSelectedFrame();
        if (!(frame instanceof LispFrame))
            throw new WrongTypeArgumentException("frame-live-p", frame);
        try {
            //todo: if noRecord -- don't change buffers' ans windows' order
            ((LispFrame)frame).getBufferManager().switchToWindow(window);
            return window;
        } catch (LispException e) {
            throw new NoWindowException((LispFrame) frame, window);
        }
    }

    @Subroutine("select-window")
    public static LispWindow selectWindow (Environment environment, LispWindow window, @Optional LispObject noRecord) {
        //todo: if noRecord, don't put window and it's buffer to the top of appropriate (window/buffer) global ring
        environment.setBufferCurrentForEditing(getBufferByWindow(environment, window));
        LispFrame frame = getFrame(environment, window);
        environment.setSelectedFrame(frame);
        return setFrameSelectedWindow(environment, frame, window, noRecord);
    }

    @Subroutine("window-buffer")
    public static LispBuffer windowBuffer (Environment environment, @Optional LispObject window) {
        return getBufferByWindow(environment, getWindow(environment, window));
    }

    @Subroutine(value = "delete-other-windows", isCmd = true, key = "\\C-x1")
    public static LispSymbol deleteOtherWindows (Environment environment, @Optional LispObject window) {
        window = getWindow(environment, window);
        if (isMinibufferWindow(environment, (LispWindow) window)) {
            throw new LispException(JelispBundle.message("minibuffer.window.cannot.fill.frame"));
        }
        LispFrame frame = getFrame(environment, (LispWindow) window);
        frame.deleteOtherWindows((LispWindow) window);
        return LispSymbol.ourNil;
    }
}
