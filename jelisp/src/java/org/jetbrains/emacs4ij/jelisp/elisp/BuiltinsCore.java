package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCore {
    private BuiltinsCore() {}

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
    public static LObject set (Environment environment, LispSymbol variable, LObject initValue) {
        LObject value = (initValue == null) ? LispSymbol.ourVoid : initValue;
        variable.setValue(value);
        environment.setVariable(variable);
        return value;
    }

    @Subroutine("equal")
    public static LispObject equal (LObject one, LObject two) {
        if (one.equals(two))
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }

    /* eq returns t if object1 and object2 are integers with the same value.
    Also, since symbol names are normally unique, if the arguments are symbols with the same name, they are eq.
    For other types (e.g., lists, vectors, strings), two arguments with the same contents or elements are not necessarily eq to each
    other: they are eq only if they are the same object, meaning that a change in the contents of one will be reflected by the
    same change in the contents of the other.
    * */

    @Subroutine("eq")
    public static LispObject eq (LObject one, LObject two) {
        if (one == two) return LispSymbol.ourT;
        if (one.getClass() != two.getClass()) return LispSymbol.ourNil;
        if (one instanceof LispNumber) {
            return (((LispNumber) one).getData()  == ((LispNumber) two).getData()) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (one instanceof LispSymbol) {
            return ((LispSymbol) one).getName().equals(((LispSymbol) two).getName()) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if ((one instanceof LispString) && (((LispString) one).getData().equals(""))) {
            return ((LispString) two).getData().equals("") ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        return LispSymbol.ourNil;
    }

    @Subroutine("null")
    public static LispObject lispNull (LObject lObject) {
        return lObject.equals(LispSymbol.ourNil) ? LispSymbol.ourT : LispSymbol.ourNil;
    }

    @Subroutine("not")
    public static LispObject lispNot (LObject lObject) {
        return lispNull(lObject);
    }

    @Subroutine("call-interactively")
    public static LObject callInteractively (Environment environment, LispSymbol function, @Optional LObject recordFlag, LObject keys) {
        if (!BuiltinsCheck.commandp(environment, function, null).equals(LispSymbol.ourT))
            throw new WrongTypeArgument("commandp", function.getName());
        //read args
        //assign args
        //invoke function
        return null;

    }

}