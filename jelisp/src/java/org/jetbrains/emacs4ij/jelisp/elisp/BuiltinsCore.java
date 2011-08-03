package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCore {
    private BuiltinsCore() {}

    @Subroutine(value = "+")
    public static LispObject plus (Environment environment, List<LispObject> args) {
        int ans = 0;
        for (LispObject lispObject: args) {
            if (lispObject.equals(LispSymbol.ourNil))
                break;
            ans += ((LispInteger)lispObject).getMyData();
        }
        return new LispInteger(ans);
    }
    @Subroutine(value = "*")
    public static LispObject multiply (Environment environment, List<LispObject> args) {
        int ans = 1;
        for (LispObject lispObject: args) {
            ans *= ((LispInteger)lispObject).getMyData();
        }
        return new LispInteger(ans);
    }
    @Subroutine(value = "set", exact = 2)
    public static LispObject set (Environment environment, List<LispObject> args) {
        LispSymbol variable;
        String varName = ((LispSymbol)args.get(0)).getName();
        try {
            variable = environment.find(varName);
            if (!variable.getValue().equals(args.get(1))) {
                variable.setValue(args.get(1));
                environment.defineSymbol(variable);
            }
        } catch (RuntimeException e) {
            variable = new LispSymbol(varName, args.get(1));
            environment.defineSymbol(variable);
        }
        return variable.getValue();
    }
    @Subroutine(value = "eq", exact = 2)
    public static LispObject eq (Environment environment, List<LispObject> args) {
        if (args.get(0).equals(args.get(1)))
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }
    @Subroutine(value = "null", exact = 1)
    public static LispObject lispNull (Environment environment, List<LispObject> args) {
        return (args.get(0) == LispSymbol.ourNil) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @Subroutine(value = "not", exact = 1)
    public static LispObject lispNot (Environment environment, List<LispObject> args) {
        return lispNull(environment, args);
    }
}