package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCheck {
    private BuiltinsCheck() {}

    @Subroutine(value = "stringp", exact = 1)
    public static LispObject stringp (Environment environment, List<LispObject> args) {
        return (args.get(0) instanceof LispString) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine(value = "symbolp", exact = 1)
    public static LispObject symbolp (Environment environment, List<LispObject> args) {
        return (args.get(0) instanceof LispSymbol) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine(value = "integerp", exact = 1)
    public static LispObject integerp (Environment environment, List<LispObject> args) {
        return (args.get(0) instanceof LispInteger) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine(value = "subrp", exact = 1)
    public static LispObject subrp (Environment environment, List<LispObject> args) {
        if (!(args.get(0) instanceof LispSymbol))
            return LispSymbol.ourNil;
        return ((LispSymbol)(args.get(0))).is(LispSymbol.FunctionType.BuiltIn) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine(value = "bufferp", exact = 1)
    public static LispObject bufferp (Environment environment, List<LispObject> args) {
        return (args.get(0) instanceof LispBuffer) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
}
