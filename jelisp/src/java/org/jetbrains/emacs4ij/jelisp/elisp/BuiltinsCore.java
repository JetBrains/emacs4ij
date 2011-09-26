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

    //todo: accept FLOAT and MARKERS
    @Subroutine("+")
    public static LispInteger plus (@Optional List<LispInteger> args) {
        int ans = 0;
        if (args != null) {
            for (LispInteger lispObject: args) {
                ans += lispObject.getData();
            }
        }
        return new LispInteger(ans);
    }
    //todo: accept FLOAT and MARKERS
    @Subroutine("*")
    public static LispInteger multiply (List<LispInteger> args) {
        int ans = 1;
        for (LispInteger lispObject: args) {
            ans *= lispObject.getData();
        }
        return new LispInteger(ans);
    }

    @Subroutine("set")
    public static LObject set (Environment environment, LispSymbol variable, LObject value) {
        LispSymbol envVar = environment.find(variable.getName());
        if (envVar == null) {
            environment.defineSymbol(new LispSymbol(variable.getName(), value));
        } else if (!envVar.getValue().equals(value)) {
            envVar.setValue(value);
            environment.defineSymbol(envVar);
        }
        return value;
    }

    @Subroutine("eq")
    public static LispObject eq (LObject one, LObject two) {
        if (one.equals(two))
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    @Subroutine(value = "null")
    public static LispObject lispNull (LObject lObject) {
        return lObject.equals(LispSymbol.ourNil) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine(value = "not")
    public static LispObject lispNot (LObject lObject) {
        return lispNull(lObject);
    }
}