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

    private static LispBuffer getBufferByBufferNameOrNil (Environment environment, @Optional LObject bufferOrName) {
        if (bufferOrName == null || bufferOrName.equals(LispSymbol.ourNil))
            return environment.getBufferCurrentForEditing();
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer == null) {
                throw new NoBufferException(((LispString) bufferOrName).getData());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispBuffer)
            return (LispBuffer) bufferOrName;
        throw new WrongTypeArgument("buffer-or-name", bufferOrName.getClass().getSimpleName());
    }

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

    @Subroutine("get-buffer-create")
    public static LObject getBufferCreate (Environment environment, LObject bufferOrName) {
        if (bufferOrName instanceof LispBuffer)
            return bufferOrName;
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer != null)
                return buffer;
            return environment.createBuffer(((LispString) bufferOrName).getData());
        }
        throw new WrongTypeArgument("buffer-or-name", bufferOrName.getClass().getSimpleName());
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

    @Subroutine("buffer-list")
    public static LObject bufferList (Environment environment, @Optional LObject frame) {
        //TODO: If frame is a frame, this returns frame's local buffer list.
        // If frame is nil or omitted, the fundamental buffer list is used: the buffers appear in order of most recent display or selection, regardless of which frames they were displayed on.
        return environment.getBufferList();
    }

    //todo: it is a command
    @Subroutine("bury-buffer")
    public static LObject buryBuffer (Environment environment, @Optional LObject bufferOrName) {
        LispBuffer buffer;
        if (bufferOrName == null) {
            buffer = environment.getBufferCurrentForEditing();
        } else if (bufferOrName instanceof LispString) {
            buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer == null) {
                throw new NoBufferException(((LispString) bufferOrName).getData());
            }
        } else if (bufferOrName instanceof LispBuffer) {
            buffer = (LispBuffer) bufferOrName;
        } else {
            throw new WrongTypeArgument("stringp", bufferOrName.toString());
        }

        if (environment.getBufferCurrentForEditing().equals(buffer)) {
            switchToBuffer(environment, LispSymbol.ourNil, null);
        }

        environment.buryBuffer(buffer);

        return LispSymbol.ourNil;
    }

    /*
    unbury-buffer

    This command switches to the last buffer in the local buffer list of the selected frame.
    More precisely, it calls the function switch-to-buffer (see Displaying Buffers), to display the buffer returned by last-buffer, see above, in the selected window.
     */
    //todo: it is a command
    @Subroutine("unbury-buffer")
    public static LObject unburyBuffer (Environment environment) {
        LispBuffer lastBuffer = lastBuffer(environment, null, null, null);
        switchToBuffer(environment, lastBuffer, null);
        return lastBuffer;
    }

    //todo: it's a compiled lisp function
    @Subroutine("last-buffer")
    public static LispBuffer lastBuffer (Environment environment, @Optional LObject buffer, LObject visibleOk, LObject frame) {
        if (buffer == null || !(buffer instanceof LispBuffer)) {
            return environment.lastBuffer();
        }
        return environment.lastBuffer(((LispBuffer) buffer).getName());
    }

    @Subroutine("generate-new-buffer-name")
    public static LispString generateNewBufferName (Environment environment, LispString startingName, @Optional LispString ignore) {
        String result = startingName.getData();
        for (int n = 2; ; ++n) {
            if (!environment.containsBuffer(result))
                return new LispString(result);
            else if (ignore != null && result.equals(ignore.getData()))
                return ignore;
            result = startingName.getData() + '<' + n + '>';
        }
    }

    @Subroutine("generate-new-buffer")
    public static LispBuffer generateNewBuffer (Environment environment, LispString startingName) {
        LispString name = generateNewBufferName(environment, startingName, null);
        return environment.createBuffer(name.getData());
    }

    //todo: interactive
    @Subroutine("replace-buffer-in-windows")
    public static LObject replaceBufferInWindows (Environment environment, @Optional LObject bufferOrName) {
        //todo: replace given buffer in all windows where it is opened
        if (bufferOrName == null)
            bufferOrName = environment.getBufferCurrentForEditing();
        switchToBuffer(environment, otherBuffer(environment, bufferOrName), null);
        return LispSymbol.ourNil;
    }


    //todo: interactive, bound to C-x k
    @Subroutine("kill-buffer")
    public static LObject killBuffer (Environment environment, @Optional LObject bufferOrName) {
        replaceBufferInWindows(environment, bufferOrName);

        LispSymbol killBufferQueryFunctions = environment.find("kill-buffer-query-functions");
        if (killBufferQueryFunctions != null) {
            LObject functions = killBufferQueryFunctions.getValue();
            if (functions != null && functions != LispSymbol.ourVoid) {
                LObject evaluationResult = functions.evaluate(environment);
                if (evaluationResult.equals(LispSymbol.ourNil))
                    return new LispString("The buffer " + bufferOrName + " was not killed due to kill-buffer-query-functions.");
            }
        }
        //todo: run hooks
        //todo: Any processes that have this buffer as the `process-buffer' are killed with SIGHUP.

        LispBuffer buffer = getBufferByBufferNameOrNil(environment, bufferOrName);
        //todo: check if modified. If user decides not to kill the buffer, return nil

        environment.getMainEnvironment().setSelectionManagedBySubroutine(true);
        environment.getMainEnvironment().killBuffer(buffer);
        return LispSymbol.ourT;
    }

}
