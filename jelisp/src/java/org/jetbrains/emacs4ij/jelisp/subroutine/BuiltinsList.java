package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

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

    private static boolean isCons (LObject object) {
        return object instanceof LispList;
    }

    @Subroutine("consp")
    public static LispSymbol consp (LObject object) {
        return LispSymbol.bool(isCons(object));
    }
    
    private static boolean isList (LObject object) {
        return object instanceof LispList || object.equals(LispSymbol.ourNil);    
    } 

    @Subroutine("listp")
    public static LispSymbol listp (LObject object) {
        return LispSymbol.bool(isList(object));
    }

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
        if (isCons(arg))
            return ((LispList)arg).car();
        return LispSymbol.ourNil;
    }
    
    @Subroutine("cdr-safe")
    public static LObject cdrSafe (LObject arg) {
        if (isCons(arg))
            return ((LispList) arg).cdr();
        return LispSymbol.ourNil;
    }

    @Subroutine("member")
    public static LispObject member (LObject element, LispList list) {
        return list.memq(element, "equal");
    }

    @Subroutine("memq")
    public static LispObject memq (LObject element, LispList list) {
        return list.memq(element, "eq");
    }
    
    @Subroutine("list")
    public static LispObject list (@Optional LObject... args) {
        if (args == null)
            return LispSymbol.ourNil;
        return LispList.list(args);
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
        if (lists == null || lists.length == 0)
            return LispSymbol.ourNil;
        if (lists.length == 1)
            return lists[0];
        for (int i = lists.length - 2; i > -1; --i) {
            if (!(lists[i] instanceof LispList || lists[i].equals(LispSymbol.ourNil)))
                throw new WrongTypeArgumentException("list", lists[i].toString());

            if (lists[i].equals(LispSymbol.ourNil))
                lists[i] = lists[i+1];
            else
                ((LispList)lists[i]).append(lists[i+1]);
        }
        return lists[0];
    }
    
    @Subroutine("nth")
    public static LObject nthElement (LispInteger n, LispList list) {
        List<LObject> elements = list.toLObjectList();
        int index = n.getData();
        if (elements.size() < index)
            return LispSymbol.ourNil;
        if (index < 0)
            return elements.get(0);
        return elements.get(index);
    }
    
    @Subroutine("assoc")
    public static LObject assoc (LObject key, LObject list) {
        if (!isList(list))
            throw new WrongTypeArgumentException("listp", list.toString());
        if (list.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        for (LObject element : ((LispList)list).toLObjectList()) {
            if (element instanceof LispList) {
                if (key.equals(((LispList) element).car()))
                    return element;
            }
        }
        return LispSymbol.ourNil;
    }
    
    @Subroutine("setcdr")
    public static LObject setCdr (LObject cell, LObject newCdr) {
        if (!isCons(cell))
            throw new WrongTypeArgumentException("consp", cell.toString());
        ((LispList)cell).setCdr(newCdr);
        return newCdr;
    }

    @Subroutine("delq")
    public static LObject delq (LObject element, LObject list) {
        if (!isCons(list))
            throw new WrongTypeArgumentException("listp", list.toString());
        return ((LispList)list).delq(element);
    }

    @Subroutine("nthcdr")
    public static LObject nthCdr (LispInteger n, LObject list) {
        if (list.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        if (!(list instanceof LispList))
            throw new WrongTypeArgumentException("listp", list.toString());
        LObject result = list;
        int i = 0;
        for (; i < n.getData() && result instanceof LispList; ++i) {
            result = ((LispList)result).cdr();
        }
        if (i < n.getData() && !LispSymbol.ourNil.equals(result))
            throw new WrongTypeArgumentException("listp", result.toString());
        return result;
    }
    
    @Subroutine("assq")
    public static LObject assq (LObject key, LObject list) {
        if (!isList(list))
            throw new WrongTypeArgumentException("listp", list.toString());
        if (list.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        return ((LispList)list).assq(key);
    }

}
