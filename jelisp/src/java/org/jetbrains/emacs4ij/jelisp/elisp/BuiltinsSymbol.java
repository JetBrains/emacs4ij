package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;

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
    @Subroutine("symbol-function")
    public static LObject symbolFunction(Environment environment, LispSymbol arg) {
        try {
            LispSymbol real = environment.find(arg.getName());
            return real.getFunction();
        } catch (RuntimeException e) {
            throw new VoidFunctionException(arg.getName());
        }
    }

    @Subroutine("get")
    public static LObject get(Environment environment, LispSymbol symbol, LispSymbol propertyName) {
        return environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, propertyName);
    }

    @Subroutine("put")
    public static LObject put(Environment environment, LispSymbol symbol, LispSymbol propertyName, LObject value) {
        environment.find(symbol.getName(), "setProperty", new Class[] {LispSymbol.class, LispObject.class}, propertyName, value);
        return value;
    }
}
