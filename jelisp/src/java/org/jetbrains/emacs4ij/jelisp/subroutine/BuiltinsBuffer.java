package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.MarkerPointsNowhereException;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.isCharOrString;
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

    @Subroutine(value = "switch-to-buffer", isCmd = true, interactive = "BSwitch to buffer", key = "\\C-xb")
    public static LispObject switchToBuffer (Environment environment,
                                             LispObject bufferOrName,
                                             @Nullable @Optional LispObject noRecordObject) {
        boolean noRecord = false;
        if (noRecordObject != null) {
            if (!(noRecordObject.equals(LispSymbol.ourNil)))
                noRecord = true;
        }
        environment.setSelectionManagedBySubroutine(true);
        if (bufferOrName.equals(LispSymbol.ourNil)) {
            LispBuffer buffer = environment.getOtherBuffer();
            buffer.setActive();
            if (!noRecord) {
                environment.switchToBuffer(buffer.getName());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispString) {
            LispBuffer buffer = environment.findBuffer(((LispString) bufferOrName).getData());
            if (buffer == null) {
                return new LispString(JelispBundle.message("cannot.create.buffer"));
            }
            buffer.setActive();
            if (!noRecord) {
                environment.switchToBuffer(buffer.getName());
            }
            return buffer;
        }
        if (bufferOrName instanceof LispBuffer) {
            //todo:  If the selected window is the minibuffer window or dedicated to its buffer, use `pop-to-buffer' for displaying the buffer.
            ((LispBuffer)bufferOrName).setActive();
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
    public static LispObject gotoChar (Environment environment, MarkerOrInteger pos) {
        if (pos.getPosition() == null)
            throw new MarkerPointsNowhereException();
        environment.getBufferCurrentForEditing().gotoChar(pos.getPosition());
        return pos;
    }

    @Subroutine(value = "forward-char", isCmd = true, key = "\\C-f")
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

    @Subroutine(value = "backward-char", isCmd = true, key = "\\C-b")
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

    @Subroutine(value = "bury-buffer", isCmd = true)
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

    @Subroutine(value = "kill-buffer", isCmd = true, interactive = "bKill buffer", key = "\\C-xk")
    public static LispObject killBuffer (Environment environment, @Optional LispObject bufferOrName) {
        replaceBufferInWindows(environment, bufferOrName);
        LispSymbol killBufferQueryFunctions = environment.find("kill-buffer-query-functions");
        if (killBufferQueryFunctions != null) {
            LispObject functions = killBufferQueryFunctions.getValue();
            if (functions != null && functions != LispSymbol.ourVoid) {
                LispObject evaluationResult = functions.evaluate(environment);
                if (evaluationResult.equals(LispSymbol.ourNil))
                    return new LispString(JelispBundle.message("buffer.not.killed", bufferOrName.toString()));
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

    private static LispObject evaluateString (Environment environment, String code) {
        return new ForwardParser().parseLine(code).evaluate(environment);
    }

    @Subroutine("insert")
    public static LispSymbol insert (Environment environment, @Optional LispObject... args) {
        StringBuilder toInsert = new StringBuilder();
        for (LispObject arg: args) {
            if (!isCharOrString(arg))
                throw new WrongTypeArgumentException("char-or-string-p", arg);
            if (arg instanceof LispInteger) {
                toInsert.append(((LispInteger) arg).toCharacterString());
                continue;
            }
            LispObject kbd = evaluateString(environment, "(kbd " + arg.toString() + ")");
            String converted = kbd instanceof LispString ? ((LispString) kbd).getData() : kbd.toString();
//            if (!converted.equals(((LispString)arg).getData()))
            toInsert.append(converted);//.append(" ");
        }
//        toInsert.deleteCharAt(toInsert.length() - 1);
        environment.getBufferCurrentForEditing().insert(toInsert.toString());
        return LispSymbol.ourNil;
    }

    @Subroutine("set-buffer-major-mode")
    public static LispObject setBufferMajorMode (Environment environment, LispBuffer buffer) {
        //if buffer == scratch => set value from "initial-major-mode". But I have no scratch :)
        LispSymbol mode = environment.find("major-mode");
        return BuiltinsCore.functionCall(environment, mode.getValue());
    }
}
