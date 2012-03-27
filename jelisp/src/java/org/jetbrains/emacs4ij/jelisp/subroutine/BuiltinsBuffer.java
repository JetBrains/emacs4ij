package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.isNil;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:30 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsBuffer {
    private BuiltinsBuffer() {}

    public static LispBuffer getBufferByBufferNameOrNil (Environment environment, @Optional LispObject bufferOrName) {
        if (isNil(bufferOrName))
            return environment.getBufferCurrentForEditing();
        if (bufferOrName instanceof LispString) {
            return environment.findBufferSafe(((LispString) bufferOrName).getData());
        }
        if (bufferOrName instanceof LispBuffer)
            return (LispBuffer) bufferOrName;
        throw new WrongTypeArgumentException("stringp", bufferOrName);
    }

    @Subroutine(value = "current-buffer")
    public static LispBuffer getCurrentBuffer(Environment environment) {
        return environment.getBufferCurrentForEditing();
    }

    @Subroutine("buffer-size")
    public static LispObject bufferSize(Environment environment, @Optional LispObject buffer) {
        if (isNil(buffer))
            buffer = environment.getBufferCurrentForEditing();
        if (!(buffer instanceof LispBuffer))
            throw new WrongTypeArgumentException("bufferp", buffer);
        return new LispInteger(((LispBuffer)buffer).getSize());
    }

    @Subroutine("buffer-name")
    public static LispObject bufferName (Environment environment, @Optional LispObject buffer) {
        if (isNil(buffer))
            buffer = environment.getBufferCurrentForEditing();
        if (!(buffer instanceof LispBuffer))
            throw new WrongTypeArgumentException("bufferp", buffer);
        return new LispString(((LispBuffer)buffer).getName());
    }

    @Subroutine("get-buffer")
    public static LispObject getBuffer (Environment environment, LispObject bufferOrName) {
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer == null)
                return LispSymbol.ourNil;
            return buffer;
        }
        if (bufferOrName instanceof LispBuffer) {
            return bufferOrName;
        }
        throw new WrongTypeArgumentException("stringp", bufferOrName);
    }

    @Subroutine("get-buffer-create")
    public static LispObject getBufferCreate (Environment environment, LispObject bufferOrName) {
        if (bufferOrName instanceof LispBuffer)
            return bufferOrName;
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer != null)
                return buffer;
            return environment.createBuffer(((LispString) bufferOrName).getData());
        }
        throw new WrongTypeArgumentException("stringp", bufferOrName);
    }

    // todo:(other-buffer &optional BUFFER VISIBLE-OK FRAME)
    @Subroutine("other-buffer")
    public static LispBuffer otherBuffer (Environment environment, @Optional LispObject buffer) {
        if (buffer == null || !(buffer instanceof LispBuffer))
            return environment.getOtherBuffer();
        return environment.getOtherBuffer((LispBuffer)buffer);
    }

    @Subroutine("set-buffer")
    public static LispObject setBuffer (Environment environment, LispObject bufferOrName) {
        LispObject lispObject = getBuffer(environment, bufferOrName);
        if (lispObject.equals(LispSymbol.ourNil)) {
            throw new NoBufferException(bufferOrName.toString());
        }
        if (!environment.isMainOrGlobal()) {
            environment.setBufferCurrentForEditing((LispBuffer)lispObject);
        }
        return lispObject;
    }

    //todo: bound to C-x b, <menu-bar> <buffer> <select-named-buffer>
    @Subroutine(value = "switch-to-buffer", isCmd = true, interactive = "BSwitch to buffer")
    public static LispObject switchToBuffer (Environment environment, LispObject bufferOrName, @Optional LispObject noRecordObject) {
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
        throw new WrongTypeArgumentException("stringp", bufferOrName);
    }

    @Subroutine("point")
    public static LispInteger point (Environment environment) {
        return new LispInteger(environment.getBufferCurrentForEditing().point());
    }

    @Subroutine("point-min")
    public static LispObject pointMin (Environment environment) {
        return new LispInteger(environment.getBufferCurrentForEditing().pointMin());
    }

    @Subroutine("point-max")
    public static LispObject pointMax (Environment environment) {
        return new LispInteger(environment.getBufferCurrentForEditing().pointMax());
    }

    @Subroutine(value = "goto-char", isCmd = true, interactive = "nGoto char: ")
    public static LispObject gotoChar (Environment environment, LispObject pos) {
        if (!BuiltinPredicates.isIntegerOrMarker(pos))
            throw new WrongTypeArgumentException("integer-or-marker-p", pos.toString());
        int data = (Integer)BuiltinArithmetic.numberOrMarkerToNumber(pos).getData();
        environment.getBufferCurrentForEditing().gotoChar(data);
        return pos;
    }

    //todo: bound to C-f, <right>
    @Subroutine(value = "forward-char", isCmd = true, interactive = "")
    public static LispObject forwardChar (Environment environment, @Optional LispObject shift) {
        if (isNil(shift))
            shift = new LispInteger(1);
        if (!(shift instanceof LispInteger))
            throw new WrongTypeArgumentException("integerp", shift.toString());
        String message = environment.getBufferCurrentForEditing().forwardChar(((LispInteger)shift).getData());
        if (message.equals(""))
            return LispSymbol.ourNil;
        return new LispSymbol(message);
    }

    //todo: bound to C-b, <left>
    @Subroutine(value = "backward-char", isCmd = true, interactive = "")//, key = "\\C-b")
    public static LispObject backwardChar (Environment environment, @Optional LispObject shift) {
        if (isNil(shift))
            shift = new LispInteger(1);
        if (!(shift instanceof LispInteger))
            throw new WrongTypeArgumentException("integerp", shift.toString());
        String message = environment.getBufferCurrentForEditing().forwardChar(-((LispInteger)shift).getData());
        if (message.equals(""))
            return LispSymbol.ourNil;
        return new LispSymbol(message);
    }

    @Subroutine("buffer-list")
    public static LispObject bufferList (Environment environment, @Optional LispObject frame) {
        //TODO: If frame is a frame, this returns frame's local buffer list.
        // If frame is nil or omitted, the fundamental buffer list is used: the buffers appear in order of most recent display or selection, regardless of which frames they were displayed on.
       
        if (frame == null || frame.equals(LispSymbol.ourNil) || !(frame instanceof LispFrame)) {
            //use fundamental buffer list

        } else {


        }

        return environment.getBufferList();
    }

    @Subroutine(value = "bury-buffer", isCmd = true, interactive = "")
    public static LispObject buryBuffer (Environment environment, @Optional LispObject bufferOrName) {
        LispBuffer buffer;
        if (isNil(bufferOrName)) {
            buffer = environment.getBufferCurrentForEditing();
        } else if (bufferOrName instanceof LispString) {
            buffer = environment.findBufferSafe(((LispString) bufferOrName).getData());
        } else if (bufferOrName instanceof LispBuffer) {
            buffer = (LispBuffer) bufferOrName;
        } else {
            throw new WrongTypeArgumentException("stringp", bufferOrName);
        }

        if (environment.getBufferCurrentForEditing().equals(buffer)) {
            switchToBuffer(environment, LispSymbol.ourNil, null);
        }

        environment.buryBuffer(buffer);

        return LispSymbol.ourNil;
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

    @Subroutine(value = "replace-buffer-in-windows", isCmd = true, interactive = "bReplace buffer in windows")
    public static LispObject replaceBufferInWindows (Environment environment, @Optional LispObject bufferOrName) {
        //todo: replace given buffer in all windows where it is opened
        if (bufferOrName == null)
            bufferOrName = environment.getBufferCurrentForEditing();
        switchToBuffer(environment, otherBuffer(environment, bufferOrName), null);
        return LispSymbol.ourNil;
    }

    //todo: interactive, bound to C-x k
    @Subroutine(value = "kill-buffer", isCmd = true, interactive = "bKill buffer")
    public static LispObject killBuffer (Environment environment, @Optional LispObject bufferOrName) {
        replaceBufferInWindows(environment, bufferOrName);

        LispSymbol killBufferQueryFunctions = environment.find("kill-buffer-query-functions");
        if (killBufferQueryFunctions != null) {
            LispObject functions = killBufferQueryFunctions.getValue();
            if (functions != null && functions != LispSymbol.ourVoid) {
                LispObject evaluationResult = functions.evaluate(environment);
                if (evaluationResult.equals(LispSymbol.ourNil))
                    return new LispString("The buffer " + bufferOrName + " was not killed due to kill-buffer-query-functions.");
            }
        }
        //todo: run hooks
        //todo: Any processes that have this buffer as the `process-buffer' are killed with SIGHUP.

        LispBuffer buffer = getBufferByBufferNameOrNil(environment, bufferOrName);
        //todo: check if modified. If user decides not to kill the buffer, return nil

       // environment.getMainEnvironment().setSelectionManagedBySubroutine(true);
       // environment.getMainEnvironment().killBuffer(buffer);

        environment.setSelectionManagedBySubroutine(true);
        GlobalEnvironment.INSTANCE.killBuffer(buffer);

        return LispSymbol.ourT;
    }

    @Subroutine("minibuffer-depth")
    public static LispInteger minibufferDepth (Environment environment) {
        return new LispInteger(environment.getMiniBufferActivationsDepth());
    }
    
    public static boolean isSyntaxTable (LispObject object) {
        //todo: true if char-table
        return false;
    }
    
    @Subroutine("syntax-table-p")
    public static LispSymbol syntaxTableP (LispObject object) {
        return LispSymbol.bool(isSyntaxTable(object));    
    }
    
    @Subroutine("set-syntax-table")
    public static void setSyntaxTable (LispObject syntaxTable) {
        if (!isSyntaxTable(syntaxTable))
            throw new WrongTypeArgumentException("syntax-table-p", syntaxTable.toString());
        //todo ?
    }
}
