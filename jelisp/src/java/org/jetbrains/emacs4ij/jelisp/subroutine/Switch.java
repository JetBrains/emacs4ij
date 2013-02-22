package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.NoWindowException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/25/12
 * Time: 5:03 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Switch {
    private Switch() {}

    /**
     * switches environment's "buffer ring" to buffer
     */
    private static LispBuffer switchBuffers (Environment environment, LispBuffer buffer, boolean noRecord) {
        buffer.setActive();
//        environment.setBufferCurrentForEditing(buffer);
        if (!noRecord) {
            environment.switchToBuffer(buffer);
        }
        return buffer;
    }

    @Subroutine(value = "switch-to-buffer", isCmd = true, interactive = "BSwitch to buffer", key = "\\C-xb")
    public static LispObject switchToBuffer (Environment environment, LispObject bufferOrName,
                                             @Nullable @Optional LispObject noRecordObject) {
        boolean noRecord = !Predicate.isNil(noRecordObject);

        environment.setSelectionManagedBySubroutine(true);
        if (bufferOrName.equals(LispSymbol.ourNil)) {
            LispObject otherBuffer = Buffer.otherBuffer(environment, bufferOrName, LispSymbol.ourT, null);
            if (otherBuffer.equals(LispSymbol.ourNil))
                throw new InternalException(JelispBundle.message("nil.other.buffer"));
            return switchBuffers(environment, (LispBuffer) otherBuffer, noRecord);
        }
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer == null)
                return new LispString(JelispBundle.message("cannot.create.buffer"));
            return switchBuffers(environment, buffer, noRecord);
        }
        if (bufferOrName instanceof LispBuffer) {
            return switchBuffers(environment, (LispBuffer) bufferOrName, noRecord);
        }
        throw new WrongTypeArgumentException("stringp", bufferOrName);
    }

    @Subroutine("set-frame-selected-window")
    public static LispWindow setFrameSelectedWindow (Environment environment, LispObject frame,
                                                     LispWindow window, @Optional LispObject noRecord) {
        LispFrame f = Frame.getLiveFrame(environment, frame);
        if (f != window.getFrame())
            throw new InternalException(JelispBundle.message("invalid.window.frame", window.toString(),
                    window.getFrame().toString(), f.toString()));
        try {
            if (!Predicate.isNil(noRecord))
                environment.switchToWindow(window);
            window.setActive();
            return window;
        } catch (LispException e) {
            throw new NoWindowException((LispFrame) frame, window);
        }
    }

    @Subroutine("select-window")
    public static LispWindow selectWindow (Environment environment, LispWindow window, @Optional LispObject noRecord) {
        //todo: if noRecord, don't put window and it's buffer to the top of appropriate (window/buffer) global ring
        environment.setBufferCurrentForEditing(window.getBuffer());
        LispFrame frame = window.getFrame();
        environment.setSelectedFrame(frame);
        return setFrameSelectedWindow(environment, frame, window, noRecord);
    }

    @Subroutine("select-frame")
    public static LispFrame selectFrame (Environment environment, LispFrame frame, @Optional LispObject noRecord) {
        LispWindow window = environment.getFrameSelectedWindow(frame);
        if (!Predicate.isNil(noRecord))
            environment.switchToWindow(window);
        window.setActive();
        //todo: return nil if frame was deleted
        return frame;
    }
}
