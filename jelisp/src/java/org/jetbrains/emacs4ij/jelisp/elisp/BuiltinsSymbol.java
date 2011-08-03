package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
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

    @Builtin("symbol-function")
    private static LispObject symbolFunction(Environment environment, List<LispObject> args) {
        if (args.size() != 1)
            throw new WrongNumberOfArgumentsException("symbol-function");
        if (!(args.get(0) instanceof LispSymbol))
            throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString());
        try {
            return environment.find(((LispSymbol) args.get(0)).getName(), "getDefinition");
        } catch (RuntimeException e) {
            throw new VoidFunctionException(((LispSymbol)args.get(0)).getName());
        }
    }
    @Builtin("get")
    private static LispObject get(Environment environment, List<LispObject> args) {
        if (args.size() != 2)
            throw new WrongNumberOfArgumentsException("get");
        if ((!(args.get(0) instanceof LispSymbol)) || (!(args.get(1) instanceof LispSymbol)))
            throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString() + " and " + args.get(1).getClass().toString());
        LispSymbol symbol = (LispSymbol) args.get(0);
        LispSymbol property = (LispSymbol) args.get(1);
        return environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, property);
    }
    @Builtin("put")
    private static LispObject put(Environment environment, List<LispObject> args) {
        if (args.size() != 3)
            throw new WrongNumberOfArgumentsException("put");
        if ((!(args.get(0) instanceof LispSymbol)) || (!(args.get(1) instanceof LispSymbol)))
            throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString() + " and " + args.get(1).getClass().toString());
        LispSymbol symbol = (LispSymbol) args.get(0);
        LispSymbol property = (LispSymbol) args.get(1);
        LispObject value = args.get(2);
        environment.find(symbol.getName(), "setProperty", new Class[] {LispSymbol.class, LispObject.class}, property, value);
        return value;
    }
}
