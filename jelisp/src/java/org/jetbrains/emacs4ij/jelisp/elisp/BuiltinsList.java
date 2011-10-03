package org.jetbrains.emacs4ij.jelisp.elisp;

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

    @Subroutine("car")
    public static LObject car (LispList arg) {
        return arg.car();
    }
    @Subroutine("cdr")
    public static LispObject cdr (LispList arg) {
        LispList cdr = arg.cdr();
        return (cdr.isEmpty()) ? LispSymbol.ourNil : cdr;
    }
    @Subroutine("car-safe")
    public static LObject carSafe (LObject arg) {
        if (arg instanceof LispList)
            return ((LispList)arg).car();
        return LispSymbol.ourNil;
    }
    @Subroutine("cdr-safe")
    public static LispObject cdrSafe (LObject arg) {
        if (arg instanceof LispList)
            return ((LispList) arg).cdr();
        return LispSymbol.ourNil;
    }
    @Subroutine("memq")
    public static LispObject memq (LObject element, LispList list) {
        return list.memq(element);
    }
    @Subroutine("list")
    public static LispObject list (List<LObject> args) {
        LispList list = new LispList(args);
        return list.isEmpty() ? LispSymbol.ourNil : list;
    }
}
