package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 4:26 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BList {
  private BList() {}

  private static boolean isCons (LispObject object) {
    return object instanceof LispList;
  }

  @Subroutine("consp")
  public static LispSymbol consp (LispObject object) {
    return LispSymbol.bool(isCons(object));
  }

  private static boolean isList (LispObject object) {
    return object instanceof LispList || object.equals(LispSymbol.NIL);
  }

  @Subroutine("listp")
  public static LispSymbol listp (LispObject object) {
    return LispSymbol.bool(isList(object));
  }

  @Subroutine("car")
  public static LispObject car (LispObject arg) {
    if (arg.equals(LispSymbol.NIL))
      return LispSymbol.NIL;
    if (arg instanceof LispList)
      return ((LispList) arg).car();
    throw new WrongTypeArgumentException("listp", arg);
  }

  @Subroutine("cdr")
  public static LispObject cdr (LispObject arg) {
    if (arg.equals(LispSymbol.NIL))
      return LispSymbol.NIL;
    if (arg instanceof LispList)
      return ((LispList) arg).cdr();
    throw new WrongTypeArgumentException("listp", arg);
  }

  @Subroutine("car-safe")
  public static LispObject carSafe (LispObject arg) {
    if (isCons(arg))
      return ((LispList)arg).car();
    return LispSymbol.NIL;
  }

  @Subroutine("cdr-safe")
  public static LispObject cdrSafe (LispObject arg) {
    if (isCons(arg))
      return ((LispList) arg).cdr();
    return LispSymbol.NIL;
  }

  @Subroutine("member")
  public static LispObject member (LispObject element, LispList list) {
    return list.memq(element, "equal");
  }

  @Subroutine("memq")
  public static LispObject memq (LispObject element, LispList list) {
    return list.memq(element, "eq");
  }

  @Subroutine("list")
  public static LispObject list (@Optional LispObject... args) {
    if (args == null)
      return LispSymbol.NIL;
    return LispList.list(args);
  }

  @Subroutine("cons")
  public static LispObject cons (LispObject car, LispObject cdr) {
    return LispList.cons(car, cdr);
  }

  @Subroutine("nreverse")
  public static LispObject nReverse (LispList list) {
    return list.nReverse();
  }

  @Subroutine("nconc")
  public static LispObject nConcatenate (@Optional LispObject... lists) {
    if (lists == null || lists.length == 0)
      return LispSymbol.NIL;
    if (lists.length == 1)
      return lists[0];
    for (int i = lists.length - 2; i > -1; --i) {
      if (!(lists[i] instanceof LispList || lists[i].equals(LispSymbol.NIL)))
        throw new WrongTypeArgumentException("list", lists[i]);

      if (lists[i].equals(LispSymbol.NIL))
        lists[i] = lists[i+1];
      else
        ((LispList)lists[i]).append(lists[i+1]);
    }
    return lists[0];
  }

  @Subroutine("nth")
  public static LispObject nthElement (LispInteger n, LispList list) {
    List<LispObject> elements = list.toLispObjectList();
    int index = n.getData();
    if (elements.size() <= index)
      return LispSymbol.NIL;
    if (index < 0)
      return elements.get(0);
    return elements.get(index);
  }

  @Subroutine("assoc")
  public static LispList assoc (LispObject key, LispObject list) {
    if (!isList(list))
      throw new WrongTypeArgumentException("listp", list);
    if (list.equals(LispSymbol.NIL))
      return LispList.list();
    for (LispObject element : ((LispList)list).toLispObjectList()) {
      if (element instanceof LispList) {
        if (key.equals(((LispList) element).car()))
          return (LispList) element;
      }
    }
    return LispList.list();
  }

  @Subroutine("setcdr")
  public static LispObject setCdr (LispObject cell, LispObject newCdr) {
    if (!isCons(cell))
      throw new WrongTypeArgumentException("consp", cell);
    ((LispList)cell).setCdr(newCdr);
    return newCdr;
  }

  @Subroutine("setcar")
  public static LispObject setCar (LispList list, LispObject newCar) {
    list.setCar(newCar);
    return newCar;
  }

  @Subroutine("delq")
  public static LispObject delq (LispObject element, LispObject list) {
    if (!isCons(list))
      throw new WrongTypeArgumentException("listp", list);
    return ((LispList)list).delq(element);
  }

  @Subroutine("nthcdr")
  public static LispObject nthCdr (LispInteger n, LispObject list) {
    if (list.equals(LispSymbol.NIL))
      return LispSymbol.NIL;
    if (!(list instanceof LispList))
      throw new WrongTypeArgumentException("listp", list);
    return Core.thisOrNil(((LispList) list).nthCdr(n.getData()));
  }

  @Subroutine("assq")
  public static LispObject assq (LispObject key, LispObject list) {
    if (!isList(list))
      throw new WrongTypeArgumentException("listp", list);
    if (list.equals(LispSymbol.NIL))
      return LispSymbol.NIL;
    return ((LispList)list).assq(key);
  }

  @Subroutine("sort")
  public static LispList sort (Environment environment, LispList list, LispObject predicate) {
    return list.sort(Sequence.verifyFunction(environment, predicate));
  }
}
