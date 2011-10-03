package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCore {
    private BuiltinsCore() {}

    @Subroutine("test")
    public static LispObject test (LispInteger a, @Optional LispInteger b, LispInteger c) {
        return null;
    }

    //todo: accept MARKERS
    @Subroutine("+")
    public static LispNumber plus (@Optional LispNumber... args) {
        double ans = 0.0;
        boolean isDouble = false;
        if (args != null) {
            for (LispNumber lispObject: args) {
                if (!isDouble && (lispObject.getData() instanceof Double))
                    isDouble = true;
                ans += (lispObject instanceof LispInteger) ? ((Integer)lispObject.getData()).doubleValue() : (Double)lispObject.getData();
            }
        }
        return isDouble ? LispNumber.newInstance(ans) : LispNumber.newInstance((int)ans);
    }
    //todo: accept MARKERS
    @Subroutine("*")
    public static LispNumber multiply (@Optional LispNumber... args) {
        double ans = 1;
        boolean isDouble = false;
        for (LispNumber lispObject: args) {
            if (!isDouble && (lispObject.getData() instanceof Double))
                    isDouble = true;
            ans *= (lispObject instanceof LispInteger) ? ((Integer)lispObject.getData()).doubleValue() : (Double)lispObject.getData();
        }
        return isDouble ? LispNumber.newInstance(ans) : LispNumber.newInstance((int)ans);
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