package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCheck {
    private BuiltinsCheck() {}

    @Subroutine("stringp")
    public static LispObject stringp (LObject arg) {
        return (arg instanceof LispString) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine("symbolp")
    public static LispObject symbolp (LObject arg) {
        return (arg instanceof LispSymbol) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine("integerp")
    public static LispObject integerp (LObject arg) {
        return (arg instanceof LispInteger) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine("subrp")
    public static LispObject subrp (LObject arg) {
        if (!(arg instanceof LispSymbol))
            return LispSymbol.ourNil;
        return ((LispSymbol)(arg)).is(LispSymbol.FunctionType.BuiltIn)  ||
                ((LispSymbol)(arg)).is(LispSymbol.FunctionType.SpecialForm)? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine("bufferp")
    public static LispObject bufferp (LObject arg) {
        return (arg instanceof LispBuffer) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
}
