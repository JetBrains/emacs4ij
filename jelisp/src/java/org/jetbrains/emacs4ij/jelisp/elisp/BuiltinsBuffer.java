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
        return environment.getCurrentBuffer();
    }

    //TODO: accept ourNil
    @Subroutine("buffer-size")
    public static LispObject bufferSize(Environment environment, @Optional LispBuffer buffer) {
        if (buffer == null || buffer.equals(LispSymbol.ourNil))
            buffer = environment.getCurrentBuffer();
        return new LispInteger(buffer.getSize());
    }

    //TODO: accept ourNil
    @Subroutine("buffer-name")
    public static LispObject bufferName (Environment environment, @Optional LispBuffer buffer) {
        if (buffer == null)
            buffer = environment.getCurrentBuffer();
        return new LispString(buffer.getName());
    }

    /* (get-buffer BUFFER-OR-NAME)
    Return the buffer named BUFFER-OR-NAME.
    BUFFER-OR-NAME must be either a string or a buffer.
    If BUFFER-OR-NAME is a string and there is no buffer with that name, return nil.
    If BUFFER-OR-NAME is a buffer, return it as given.
    */
    //TODO: LispBuffer or LispString. nil not allowed
    @Subroutine("get-buffer")
    public static LObject getBuffer (Environment environment, LObject bufferOrName) {
        return (bufferOrName instanceof LispString) ? environment.getBufferByName(bufferOrName.toString()) : bufferOrName;
    }

    /* (other-buffer &optional BUFFER VISIBLE-OK FRAME)
         Return most recently selected buffer other than BUFFER.
Buffers not visible in windows are preferred to visible buffers, unless optional second argument VISIBLE-OK is non-nil.
If the optional third argument FRAME is non-nil, use that frame's buffer list instead of the selected frame's buffer list.
If no other buffer exists, the buffer `*scratch*' is returned.
If BUFFER is omitted or nil, some interesting buffer is returned.
     */
    //TODO:  accept nil as buffer
    @Subroutine("other-buffer")
    public static LispBuffer otherBuffer (Environment environment, @Optional LispBuffer buffer) {
        if (buffer == null || buffer.equals(LispSymbol.ourNil))
            return environment.getOtherBuffer();
        return environment.getOtherBuffer(buffer.getName());
    }

    /*
    (set-buffer BUFFER-OR-NAME)
    Make buffer BUFFER-OR-NAME current for editing operations.
    BUFFER-OR-NAME may be a buffer or the name of an existing buffer.
    See also `save-excursion' when you want to make a buffer current temporarily.
    This function does not display the buffer, so its effect ends when the current command terminates.
    Use `switch-to-buffer' or `pop-to-buffer' to switch buffers permanently.
     */
    @Subroutine("set-buffer")
    public static LObject setBuffer (Environment environment, LObject bufferOrName) {
        LObject lispObject = getBuffer(environment, bufferOrName);
        if (lispObject.equals(LispSymbol.ourNil)) {
            throw new NoBufferException(bufferOrName.toString());
        }
        return lispObject;
    }

    /*
    switch-to-buffer is an interactive built-in function in `C source code'.

    It is bound to C-x b, <menu-bar> <buffer> <select-named-buffer>.

    (switch-to-buffer BUFFER-OR-NAME &optional NORECORD)

    Make BUFFER-OR-NAME current and display it in selected window.
    BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
    nil.  Return the buffer switched to.

    If BUFFER-OR-NAME is a string and does not identify an existing
    buffer, create a new buffer with that name.  Interactively, if
    `confirm-nonexistent-file-or-buffer' is non-nil, request
    confirmation before creating a new buffer.  If BUFFER-OR-NAME is
    nil, switch to buffer returned by `other-buffer'.

    Optional second arg NORECORD non-nil means do not put this buffer
    at the front of the list of recently selected ones.  This
    function returns the buffer it switched to as a Lisp object.

    If the selected window is the minibuffer window or dedicated to
    its buffer, use `pop-to-buffer' for displaying the buffer.

    WARNING: This is NOT the way to work on another buffer temporarily
    within a Lisp program!  Use `set-buffer' instead.  That avoids
    messing with the window-buffer correspondences.
     */
    //todo: interactive
    //todo: bufferOrName = LispBuffer or LispString or nil
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
                environment.setCurrentBuffer(buffer.getName());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispString) {
            LObject buffer = environment.getBufferByName(bufferOrName.toString());
            if (buffer.equals(LispSymbol.ourNil)) {
                return new LispString("It is not allowed to create files this way.");
                // todo: create a new buffer with that name.  Interactively, if`confirm-nonexistent-file-or-buffer' is non-nil, request confirmation before creating a new buffer
                //? : where to create a buffer?
            }
            ((LispBuffer)buffer).setBufferActive();
            if (!noRecord) {
                environment.setCurrentBuffer(((LispBuffer)buffer).getName());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispBuffer) {
            //todo:  If the selected window is the minibuffer window or dedicated to its buffer, use `pop-to-buffer' for displaying the buffer.
            ((LispBuffer)bufferOrName).setBufferActive();
            if (!noRecord) {
                environment.setCurrentBuffer(((LispBuffer)bufferOrName).getName());
            }
            return bufferOrName;
        }
        throw new WrongTypeArgument("LispBuffer or LispString or nil", bufferOrName.getClass().toString());
    }

    @Subroutine("point")
    public static LObject point (Environment environment) {
        return new LispInteger(environment.getCurrentBuffer().point());
    }

    @Subroutine("point-min")
    public static LObject pointMin (Environment environment) {
        return new LispInteger(environment.getCurrentBuffer().pointMin());
    }

    @Subroutine("point-max")
    public static LObject pointMax (Environment environment) {
        return new LispInteger(environment.getCurrentBuffer().pointMax());
    }

    @Subroutine("buffer-end")
    public static LObject bufferEnd (Environment environment, LispNumber arg) {
        return new LispInteger(environment.getCurrentBuffer().bufferEnd((Double)arg.getData()));
    }

    //todo: interactive, accepts integer OR MARKER
    @Subroutine("goto-char")
    public static LObject gotoChar (Environment environment, LispInteger pos) {
        environment.getCurrentBuffer().gotoChar(pos.getData());
        return pos;
    }

    //todo: interactive, bound to C-f, <right>
    @Subroutine("forward-char")
    public static LObject forwardChar (Environment environment, @Optional LispInteger shift) {
        if (shift == null) {
            shift = new LispInteger(1);
        }
        String message = environment.getCurrentBuffer().forwardChar(shift.getData());
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
        String message = environment.getCurrentBuffer().forwardChar(-shift.getData());
        if (message.equals(""))
            return LispSymbol.ourNil;
        return new LispSymbol(message);
    }

}
