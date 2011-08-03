package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:02 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsTypeCheck {
    @Builtin("stringp")
    public static LispObject stringp (Environment environment, List<LispObject> args) {
        if (args.size() != 1)
            throw new WrongNumberOfArgumentsException("stringp");
        return (args.get(0) instanceof LispString) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Builtin("symbolp")
    public static LispObject symbolp (Environment environment, List<LispObject> args) {
        if (args.size() != 1)
            throw new WrongNumberOfArgumentsException("symbolp");
        return (args.get(0) instanceof LispSymbol) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Builtin("integerp")
    public static LispObject integerp (Environment environment, List<LispObject> args) {
        if (args.size() != 1)
            throw new WrongNumberOfArgumentsException("integerp");
        return (args.get(0) instanceof LispInteger) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Builtin("subrp")
    public static LispObject subrp (Environment environment, List<LispObject> args) {
        if (args.size() != 1)
            throw new WrongNumberOfArgumentsException("subrp");
        //TODO : what returns the real symbol-function
        return ((args.get(0) instanceof LispString) && (args.get(0).toString().contains("<# subr"))) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
}
