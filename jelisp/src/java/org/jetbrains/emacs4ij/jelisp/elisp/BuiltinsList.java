package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 4:26 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsList {
    private BuiltinsList() {}

    @Subroutine(value = "car", exact = 1)
    public static LispObject car (Environment environment, List<LispObject> args) {
        if (!(args.get(0) instanceof LispList))
            throw new WrongTypeArgument("LispList", args.get(0).getClass().toString());
        return ((LispList) args.get(0)).car();
    }
    @Subroutine(value = "cdr", exact = 1)
    public static LispObject cdr (Environment environment, List<LispObject> args) {
        if (!(args.get(0) instanceof LispList))
            throw new WrongTypeArgument("LispList", args.get(0).getClass().toString());
        LispList cdr = ((LispList) args.get(0)).cdr();
        return (cdr.isEmpty()) ? LispSymbol.ourNil : cdr;
    }
    @Subroutine(value = "car-safe", exact = 1)
    public static LispObject carSafe (Environment environment, List<LispObject> args) {
        if (args.get(0) instanceof LispList)
            return ((LispList) args.get(0)).car();
        return LispSymbol.ourNil;
    }
    @Subroutine(value = "cdr-safe", exact = 1)
    public static LispObject cdrSafe (Environment environment, List<LispObject> args) {
        if (args.get(0) instanceof LispList)
            return ((LispList) args.get(0)).cdr();
        return LispSymbol.ourNil;
    }
    @Subroutine(value = "memq", exact = 2)
    public static LispObject memq (Environment environment, List<LispObject> args) {
        if (args.get(1) instanceof LispList) {
            return ((LispList) args.get(1)).memq(args.get(0));
        }
        throw new WrongTypeArgument("LispList", args.get(1).getClass().toString());
    }
    @Subroutine("list")
    public static LispObject list (Environment environment, List<LispObject> args) {
        LispList list = new LispList(args);
        return list.isEmpty() ? LispSymbol.ourNil : list;
    }
}
