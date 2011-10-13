package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:30 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsBuffer {
    private BuiltinsBuffer() {}

    @Subroutine("current-buffer")
    public static LispBuffer getCurrentBuffer(Environment environment) {
        return environment.getBufferCurrentForEditing();
    }

    @Subroutine("buffer-size")
    public static LispObject bufferSize(Environment environment, @Optional LObject buffer) {
        if (buffer == null || buffer.equals(LispSymbol.ourNil))
            buffer = environment.getBufferCurrentForEditing();
        if (!(buffer instanceof LispBuffer))
            throw new WrongTypeArgument("LispBuffer", buffer.getClass().getSimpleName());
        return new LispInteger(((LispBuffer)buffer).getSize());
    }

    @Subroutine("buffer-name")
    public static LispObject bufferName (Environment environment, @Optional LObject buffer) {
        if (buffer == null || buffer.equals(LispSymbol.ourNil))
            buffer = environment.getBufferCurrentForEditing();
        if (!(buffer instanceof LispBuffer))
            throw new WrongTypeArgument("LispBuffer", buffer.getClass().getSimpleName());
        return new LispString(((LispBuffer)buffer).getName());
    }

    @Subroutine("get-buffer")
    public static LObject getBuffer (Environment environment, LObject bufferOrName) {
        if (bufferOrName instanceof  LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString)bufferOrName).getData());
            if (buffer == null)
                return LispSymbol.ourNil;
            return buffer;
        }
        if (bufferOrName instanceof LispBuffer) {
            return bufferOrName;
        }
        throw new WrongTypeArgument("buffer or name", bufferOrName.getClass().getSimpleName());
    }

    // todo:(other-buffer &optional BUFFER VISIBLE-OK FRAME)
    @Subroutine("other-buffer")
    public static LispBuffer otherBuffer (Environment environment, @Optional LObject buffer) {
        if (buffer == null || !(buffer instanceof LispBuffer))
            return environment.getOtherBuffer();
        return environment.getOtherBuffer(((LispBuffer)buffer).getName());
    }

    @Subroutine("set-buffer")
    public static LObject setBuffer (Environment environment, LObject bufferOrName) {
        LObject lispObject = getBuffer(environment, bufferOrName);
        if (lispObject.equals(LispSymbol.ourNil)) {
            throw new NoBufferException(bufferOrName.toString());
        }
        if (!environment.isMainEnvironment()) {
            environment.setBufferCurrentForEditing((LispBuffer)lispObject);
        }
        return lispObject;
    }

    //todo: interactive, bound to C-x b, <menu-bar> <buffer> <select-named-buffer>
    @Subroutine("switch-to-buffer")
    public static LObject switchToBuffer (Environment environment, LObject bufferOrName, @Optional LObject noRecordObject) {

        boolean noRecord = false;
        if (noRecordObject != null) {
            if (!(noRecordObject.equals(LispSymbol.ourNil)))
                noRecord = true;
        }
        environment.setSelectionManagedBySubroutine(true);
        if (bufferOrName.equals(LispSymbol.ourNil)) {
            LispBuffer buffer = environment.getOtherBuffer();
            buffer.setBufferActive();
            if (!noRecord) {
                environment.switchToBuffer(buffer.getName());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer == null) {
                return new LispString("It is not allowed to create files this way.");
                // todo: create a new buffer with that name.  Interactively, if`confirm-nonexistent-file-or-buffer' is non-nil, request confirmation before creating a new buffer
                //? : where to create a buffer?
            }
            buffer.setBufferActive();
            if (!noRecord) {
                environment.switchToBuffer(buffer.getName());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispBuffer) {
            //todo:  If the selected window is the minibuffer window or dedicated to its buffer, use `pop-to-buffer' for displaying the buffer.
            ((LispBuffer)bufferOrName).setBufferActive();
            if (!noRecord) {
                environment.switchToBuffer(((LispBuffer) bufferOrName).getName());
            }
            return bufferOrName;
        }
        throw new WrongTypeArgument("LispBuffer or LispString or nil", bufferOrName.getClass().toString());
    }

    @Subroutine("point")
    public static LObject point (Environment environment) {
        return new LispInteger(environment.getBufferCurrentForEditing().point());
    }

    @Subroutine("point-min")
    public static LObject pointMin (Environment environment) {
        return new LispInteger(environment.getBufferCurrentForEditing().pointMin());
    }

    @Subroutine("point-max")
    public static LObject pointMax (Environment environment) {
        return new LispInteger(environment.getBufferCurrentForEditing().pointMax());
    }

    @Subroutine("buffer-end")
    public static LObject bufferEnd (Environment environment, LispNumber arg) {
        return new LispInteger(environment.getBufferCurrentForEditing().bufferEnd((Double)arg.getData()));
    }

    //todo: interactive, accepts integer OR MARKER
    @Subroutine("goto-char")
    public static LObject gotoChar (Environment environment, LispInteger pos) {
        environment.getBufferCurrentForEditing().gotoChar(pos.getData());
        return pos;
    }

    //todo: interactive, bound to C-f, <right>
    @Subroutine("forward-char")
    public static LObject forwardChar (Environment environment, @Optional LispInteger shift) {
        if (shift == null) {
            shift = new LispInteger(1);
        }
        String message = environment.getBufferCurrentForEditing().forwardChar(shift.getData());
        if (message.equals(""))
            return LispSymbol.ourNil;
        return new LispSymbol(message);
    }

    //todo: interactive, bound to C-b, <left>
    @Subroutine("backward-char")
    public static LObject backwardChar (Environment environment, @Optional LispInteger shift) {
        if (shift == null) {
            shift = new LispInteger(1);
        }
        String message = environment.getBufferCurrentForEditing().forwardChar(-shift.getData());
        if (message.equals(""))
            return LispSymbol.ourNil;
        return new LispSymbol(message);
    }

}
