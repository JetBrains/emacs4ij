package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 5:30 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsBuffer {
    private BuiltinsBuffer() {}

    @Subroutine(value = "current-buffer", max = 0)
    public static LispBuffer getCurrentBuffer(Environment environment, List<LispObject> args) {
        return Environment.getCurrentBuffer();
    }

    @Subroutine(value = "buffer-size", max = 1)
    public static LispObject bufferSize(Environment environment, List<LispObject> args) {
        if (args.size() == 1 && !(args.get(0) instanceof LispBuffer))
            throw new WrongTypeArgument("LispBuffer", args.get(0).getClass().toString());
        LispBuffer buffer = (args.size() == 0 || args.get(0).equals(LispSymbol.ourNil)) ? Environment.getCurrentBuffer() : (LispBuffer) args.get(0);
        if (buffer.equals(LispSymbol.ourNil))
            throw new RuntimeException("no buffer is currently opened");
        return new LispInteger(buffer.getSize());
    }

    @Subroutine(value = "buffer-name", max = 1)
    public static LispObject bufferName (Environment environment, List<LispObject> args) {
        if (args.size() == 1 && !(args.get(0) instanceof LispBuffer))
            throw new WrongTypeArgument("LispBuffer", args.get(0).getClass().toString());
        LispBuffer buffer = (args.size() == 0 || args.get(0).equals(LispSymbol.ourNil)) ? Environment.getCurrentBuffer() : (LispBuffer) args.get(0);
        return new LispString(buffer.getName());
    }

    @Subroutine(value = "get-buffer", exact = 1)
    public static LObject getBuffer (Environment environment, List<LispObject> args) {
        if (!(args.get(0) instanceof LispString))
            throw new WrongTypeArgument("LispString", args.get(0).getClass().toString());
        return Environment.getBuffer(args.get(0).toString());
    }

    @Subroutine(value = "other-buffer", max = 1)
    public static LispBuffer otherBuffer (Environment environment, List<LispObject> args) {
        if (args.size() == 1 && !(args.get(0) instanceof LispBuffer))
            throw new WrongTypeArgument("LispBuffer", args.get(0).getClass().toString());
        if (args.size() == 0)
            return Environment.getOtherBuffer();
        return Environment.getOtherBuffer(((LispBuffer)args.get(0)).getName());
    }



}
