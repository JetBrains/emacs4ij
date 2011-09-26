package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:05 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsSymbol {
    private BuiltinsSymbol() {}

    //TODO: symbol-function <subroutine> must return LispObject subroutine
    @Subroutine(value = "symbol-function", exact = 1)
    public static LObject symbolFunction(Environment environment, List<LObject> args) {
        if (!(args.get(0) instanceof LispSymbol))
            throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString());
        try {
            LispSymbol real = environment.find(((LispSymbol) args.get(0)).getName());
            return real.getFunction();
        } catch (RuntimeException e) {
            throw new VoidFunctionException(((LispSymbol)args.get(0)).getName());
        }
    }
    @Subroutine(value = "get", exact = 2)
    public static LObject get(Environment environment, List<LObject> args) {
        if ((!(args.get(0) instanceof LispSymbol)) || (!(args.get(1) instanceof LispSymbol)))
            throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString() + " and " + args.get(1).getClass().toString());
        LispSymbol symbol = (LispSymbol) args.get(0);
        LispSymbol property = (LispSymbol) args.get(1);
        return environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, property);
    }
    @Subroutine(value = "put", exact = 3)
    public static LObject put(Environment environment, List<LObject> args) {
        if ((!(args.get(0) instanceof LispSymbol)) || (!(args.get(1) instanceof LispSymbol)))
            throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString() + " and " + args.get(1).getClass().toString());
        LispSymbol symbol = (LispSymbol) args.get(0);
        LispSymbol property = (LispSymbol) args.get(1);
        LObject value = args.get(2);
        environment.find(symbol.getName(), "setProperty", new Class[] {LispSymbol.class, LispObject.class}, property, value);
        return value;
    }
}
