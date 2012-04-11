package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.NoWindowException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.List;

import static org.jetbrains.emacs4ij.jelisp.subroutine.Predicate.isNil;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 7:14 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Frame {
    private Frame() {}

    @Subroutine("selected-frame")
    public static LispObject selectedFrame () {
        return Core.thisOrNil(GlobalEnvironment.INSTANCE.getSelectedFrame());
    }

    @Subroutine("frame-parameter")
    public static LispObject frameParameter (LispObject frame, LispSymbol parameter) {
//        System.out.println("Ask for frame parameter: " + parameter.getName());
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
    public static LispObject getBufferWindow(Environment environment,
                                             @Optional LispObject bufferOrName, @Optional LispObject frame) {
        LispBuffer buffer = Buffer.getBufferByBufferNameOrNil(environment, bufferOrName);
        List<LispFrame> frames = new ArrayList<>();
        if (isNil(frame)) {
            frame = LispSymbol.ourNil;
        }
        if (frame.equals(new LispSymbol("visible"))) {
            frames = environment.getVisibleFrames(); //search all visible frames
        } else if (frame.equals(LispSymbol.ourT)) { //search all frames.
            frames = environment.getAllFrames();
        } else if (frame.equals(new LispInteger(0))) { //search visible and iconified frames.
            frames = environment.getVisibleAndIconifiedFrames();
        } else if (frame instanceof LispFrame) { //search only that frame.
            frames.add((LispFrame) frame);
        } else {
            LispObject selectedFrame = selectedFrame();
            if (!selectedFrame.equals(LispSymbol.ourNil))
                frames.add((LispFrame) selectedFrame);
        }

        for (LispFrame f: frames) {
            LispWindow window = f.getBufferWindow(buffer);
            if (window != null)
                return window;
        }

        return LispSymbol.ourNil;
    }

    @Subroutine(value = "make-frame-visible", isCmd = true)
    public static LispObject makeFrameVisible(Environment environment, @Optional LispObject frame) {
        if (isNil(frame)) {
            frame = selectedFrame();
        }
        LispSymbol frameLiveP = Predicate.frameLiveP(environment, frame);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", frame);
        ((LispFrame)frame).setVisible(true);
        return frame;
    }

    @Subroutine(value = "make-frame-invisible", isCmd = true)
    public static LispObject makeFrameInvisible(Environment environment,
                                                @Optional LispObject frame, @Optional LispObject force) {
        if (isNil(frame))
            frame = selectedFrame();
        LispSymbol frameLiveP = Predicate.frameLiveP(environment, frame);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", frame);
        if (isNil(force)) {
            //check that exists one more visible frame than that we want to hide
            int k = ((LispFrame)frame).isVisible() ? 1 : 0;
            if (environment.getVisibleAndIconifiedFrames().size() - k <= 0) {
                Core.error(JelispBundle.message("make.invisible.error"));
                return LispSymbol.ourNil;
            }
        }
        ((LispFrame)frame).setVisible(false);
        return LispSymbol.ourNil;
    }

    @Subroutine(value = "iconify-frame", isCmd = true)
    public static LispObject iconifyFrame(Environment environment, @Optional LispObject frame) {
        if (isNil(frame))
            frame = selectedFrame();
        LispSymbol frameLiveP = Predicate.frameLiveP(environment, frame);
        if (frameLiveP.equals(LispSymbol.ourNil))
            throw new WrongTypeArgumentException("frame-live-p", frame);
        ((LispFrame)frame).setIconified(true);
        return LispSymbol.ourNil;
    }

    @Subroutine("frame-list")
    public static LispList frameList (Environment environment) {
        List<LispFrame> frames = environment.getAllFrames();
        return LispList.list(frames.toArray(new LispObject[frames.size()]));
    }
    
    @Subroutine("selected-window")
    public static LispObject selectedWindow (Environment environment) {
        return Core.thisOrNil(environment.getSelectedWindow());
    }

    @Subroutine("frame-selected-window")
    public static LispObject frameSelectedWindow (Environment environment, @Optional LispObject frame) {
        if (!Predicate.isNil(frame) && !(frame instanceof LispFrame))
            throw new WrongTypeArgumentException("frame-live-p", frame);
        if (Predicate.isNil(frame))
            frame = environment.getSelectedFrame();
        if (frame == null)
            return LispSymbol.ourNil;
        return ((LispFrame)frame).getSelectedWindow();
    }

    @Subroutine("next-window")
    public static LispObject nextWindow(Environment environment, 
                                        @Optional LispObject window, LispObject considerMinibuffer, LispObject allFrames) {
        if (Predicate.isNil(window))
            window = environment.getSelectedWindow();
        if (!(window instanceof LispWindow))
            throw new WrongTypeArgumentException("window-live-p", window);
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
        int index = windows.indexOf((LispWindow)window);
        if (index == -1)
            throw new InternalException("next-window error");
        if (index == windows.size())
            return windows.get(0);
        return windows.get(index + 1);
    }
    
    @Subroutine("(set-frame-selected-window FRAME WINDOW &optional NORECORD)")
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
}
