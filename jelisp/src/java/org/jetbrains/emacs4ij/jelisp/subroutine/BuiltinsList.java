package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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
    public static LObject car (LObject arg) {
        if (arg.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        if (arg instanceof LispList)
            return ((LispList) arg).car();
        throw new WrongTypeArgumentException("listp", arg.toString());
    }

    @Subroutine("cdr")
    public static LObject cdr (LObject arg) {
        if (arg.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        if (arg instanceof LispList)
            return ((LispList) arg).cdr();
        throw new WrongTypeArgumentException("listp", arg.toString());
    }

    @Subroutine("car-safe")
    public static LObject carSafe (LObject arg) {
        if (arg instanceof LispList)
            return ((LispList)arg).car();
        return LispSymbol.ourNil;
    }
    
    @Subroutine("cdr-safe")
    public static LObject cdrSafe (LObject arg) {
        if (arg instanceof LispList)
            return ((LispList) arg).cdr();
        return LispSymbol.ourNil;
    }
    
    @Subroutine("memq")
    public static LispObject memq (LObject element, LispList list) {
        return list.memq(element);
    }
    
    @Subroutine("list")
    public static LispObject list (@Optional LObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        LispList list = LispList.list(args);
        return list.isEmpty() ? LispSymbol.ourNil : list;
    }
    
    @Subroutine("cons")
    public static LObject cons (LObject car, LObject cdr) {
        return LispList.cons(car, cdr);
    }
    
    @Subroutine("nreverse")
    public static LObject nReverse (LispList list) {
        return list.nReverse();
    }

    @Subroutine("nconc")
    public static LObject nConcatenate (@Optional LObject... lists) {
       /* if (args == null || args.length == 0)
            return LispSymbol.ourNil;
        int argnum;
        LObject tail = LispSymbol.ourNil, tem, val = LispSymbol.ourNil;
        for (argnum = 0; argnum < args.length; ++argnum) {
            tem = args[argnum];
            if (tem.equals(LispSymbol.ourNil))
                continue;
            if (val.equals(LispSymbol.ourNil))
                val = tem;
            if (argnum + 1 == args.length)
                break;
            if (!(tem instanceof LispList))
                throw new WrongTypeArgumentException("list", tem.toString());
            while (tem instanceof LispList) {
                tail = tem;
                tem = ((LispList)tail).cdr();
                //quit
            }
            tem = args[argnum + 1];
            ((LispList)tail).setCdr(tem);
            if (tem.equals(LispSymbol.ourNil))
                args[argnum + 1] = tail;
        }
        return val; */




        if (lists == null || lists.length == 0)
            return LispSymbol.ourNil;
        if (lists.length == 1)
            return lists[0];
        for (int i = lists.length - 2; i > -1; --i) {
            if (!(lists[i] instanceof LispList))
                throw new WrongTypeArgumentException("list", lists[i].toString());
            ((LispList)lists[i]).append(lists[i+1]);
        }
        return lists[0];
    }
}
