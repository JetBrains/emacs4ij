package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 1:30 PM
 * To change this template use File | Settings | File Templates.
 *
 * this class is a lisp list = (something in brackets 5 5 delimited by spaces or line breaks)
 */
public final class LispList implements LispSequence {
  private LispObject myCar = null;
  private LispObject myCdr = null;
  private Boolean isTrueList;

  public static LispList list (LispObject ... objects) {
    return new LispList(new ArrayList<>(Arrays.asList(objects)));
  }

  public static LispList list (List<LispObject> data) {
    return new LispList(data);
  }

  public static LispList testList (LispObject ... objects) {
    return new LispList(new ArrayList<>(Arrays.asList(objects)), true);
  }

  public static LispList listAsIs(List<LispObject> data) {
    return new LispList(data, true);
  }

  public static LispList cons (LispObject car, @Nullable LispObject cdr) {
    return new LispList(car, cdr);
  }

  private LispList (List<LispObject> data, boolean test) {
    if (data == null || data.size() == 0) {
      return;
    }
    if (data.get(0) == null) {
      throw new InternalException(JelispBundle.message("null.element"));
    }
    myCar = data.get(0);
    if (data.size() == 2) {
      isTrueList = false;
      myCdr = data.get(1);
      return;
    }
    if (data.size() == 1) {
      isTrueList = false;
      myCdr = null;
      return;
    }
    isTrueList = true;
    myCdr = new LispList(data.subList(1, data.size()), true);
  }

  private LispList (List<LispObject> data) {
    isTrueList = true;
    if (data == null || data.size() == 0) {
      return;
    }
    if (data.get(0) == null) {
      throw new InternalException(JelispBundle.message("null.element"));
    }
    myCar = data.get(0);
    if (data.size() == 1) {
      return;
    }
    myCdr = new LispList(data.subList(1, data.size()));
  }

  private LispList (@NotNull LispObject car, @Nullable LispObject cdr) {
    if (Predicate.isNil(cdr)) {
      myCar = car;
      isTrueList = true;
      return;
    }
    myCar = car;
    myCdr = cdr;
    isTrueList = cdr instanceof LispList && ((LispList)cdr).isTrueList();
  }

  public boolean isEmpty() {
    return myCar == null && myCdr == null;
  }

  @Override
  public LispObject delete(LispObject element) {
    LispObject result = this;
    LispList prev = null;
    for (LispObject tail = this; tail instanceof LispList; tail = ((LispList) tail).cdr()) {
      if (((LispList) tail).car().equals(element)) {
        if (prev == null)
          result = ((LispList) tail).cdr();
        else
          prev.setCdr(((LispList) tail).cdr());
      }
      else
        prev = (LispList) tail;
    }
    return result;
  }

  /**
   * @param environment
   * @return the result of function execution
   */
  @Override
  public LispObject evaluate(Environment environment) {
    if (isEmpty())
      return LispSymbol.ourNil;

    LispObject function = car();
    List<LispObject> args = myCdr instanceof LispList ? ((LispList)myCdr).toLispObjectList() : new ArrayList<LispObject>();

    if (function instanceof LispSymbol) {
      return ((LispSymbol)function).evaluateFunction(environment, VoidFunctionException.class, args);
    }
    if (function instanceof LispList) {
      function = new Lambda((LispList) function);
    }
    if (function instanceof Lambda) {
      if (!environment.areArgumentsEvaluated()) {
        for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
          args.set(i, args.get(i).evaluate(environment));
        }
      } else {
        environment.setArgumentsEvaluated(false);
      }
      if (!environment.areSpecFormsAndMacroAllowed())
        environment.setSpecFormsAndMacroAllowed(true);

      return ((Lambda)function).evaluate(environment, args);
    }
    if (function instanceof Primitive) {
      return LispSubroutine.evaluate(((Primitive)function).getName(), environment, args);
    }
    throw new InvalidFunctionException(function.toString());
  }

  private boolean isTrueList() {
    return isTrueList;
  }

  @Override
  public List<LispObject> toLispObjectList() {
    ArrayList<LispObject> list = new ArrayList<>();
    if (isEmpty())
      return list;
    LispList cell = this;
    do {
      LispObject cdr = cell.realCdr();
      if (cdr == null || (cdr instanceof LispList && cell.isTrueList())) {
        if (cell.realCar() != null)
          list.add(cell.realCar());
      } else {
        if (!cell.isTrueList()) {
          list.add(cell);
          break;
        }
        list.add(cell.car());
        list.add(cdr);
        break;
      }
      cell = (LispList) cdr;
    } while (cell != null);
    return list;
  }

  @Override
  public List<LispObject> mapCar(Environment environment, LispObject method) {
    LispObject list = this;
    ArrayList<LispObject> data = new ArrayList<>();
    while (list instanceof LispList) {
      data.add(Core.functionCall(environment, method, ((LispList) list).car()));
      list = ((LispList) list).realCdr();
    }
    if (list == null) {
      return data;
    }
    throw new WrongTypeArgumentException("listp", list);
  }

  @Override
  public String toCharString() {
    List<LispObject> list = toLispObjectList();
    String s = "";
    for (LispObject element: list) {
      if (!Predicate.isCharacter(element))
        throw new WrongTypeArgumentException("characterp", element);
      s += ((LispInteger)element).toCharacterString();
    }
    return s;
  }

  @Override
  public String toString() {
    if (isEmpty())
      return "nil";
    String string = "(" + car().toString();
    for (LispObject cdr = myCdr; cdr != null; ) {
      if (cdr instanceof LispList) {
        string += ' ' + ((LispList) cdr).car().toString();
        cdr = ((LispList) cdr).realCdr();
        continue;
      }
      string += " . " + cdr.toString();
      break;
    }
    return string + ')';
  }

  public LispObject car () {
    return Core.thisOrNil(myCar);
  }

  public LispObject cdr () {
    return Core.thisOrNil(myCdr);
  }

  private LispObject realCdr() {
    return myCdr;
  }

  private LispObject realCar() {
    return myCar;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof LispList)) {
      return o.equals(LispSymbol.ourNil) && isEmpty();
    }
    LispList lispList = (LispList) o;
    if (myCar != null ? !myCar.equals(lispList.myCar) : lispList.myCar != null)
      return false;
    if (myCdr != null ? !myCdr.equals(lispList.myCdr) : lispList.myCdr != null)
      return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = myCar != null ? myCar.hashCode() : 0;
    result = 31 * result + (myCdr != null ? myCdr.hashCode() : 0);
    return result;
  }

  public LispList memq (LispObject element, String equalityFunctionName) {
    LispObject cdr = this;
    try {
      if (equalityFunctionName.equals("eq")) {
        for (; cdr != null; cdr = ((LispList)cdr).realCdr()) {
          if (((LispList) cdr).realCar() != null && Core.eqs(element, ((LispList) cdr).realCar())) {
            return (LispList)cdr;
          }
        }
        return LispList.list();
      }

      if (equalityFunctionName.equals("equal")) {
        for (; cdr != null; cdr = ((LispList)cdr).realCdr()) {
          if (((LispList) cdr).realCar() != null && Core.equals(element, ((LispList) cdr).realCar())) {
            return (LispList)cdr;
          }
        }
        return LispList.list();
      }

      throw new InternalException(JelispBundle.message("unsupported.equality.function", equalityFunctionName));
    } catch (ClassCastException e) {
      throw new WrongTypeArgumentException("listp", cdr);
    }
  }

  public LispObject nReverse () {
    if (isEmpty())
      return this;
    LispObject prev = null;
    LispObject tail = this;
    while (tail instanceof LispList) {
      LispObject next = ((LispList)tail).realCdr();
      ((LispList) tail).setCdr(prev);
      prev = tail;
      tail = next;
    }
    if (tail != null) {
      throw new WrongTypeArgumentException("listp", this);
    }
    return prev;
  }

  public void append (LispObject object) {
    if (object.equals(LispSymbol.ourNil))
      return;
    if (myCdr != null && myCdr instanceof LispList)
      ((LispList) myCdr).append(object);
    else {
      if (myCdr == null) {
        myCdr = object;
        isTrueList = object instanceof LispList && ((LispList)object).isTrueList();
      } else
        throw new WrongTypeArgumentException("listp", myCdr);
    }
  }

  @Override
  public int size() {
    if (isEmpty())
      return 0;
    if (myCdr == null)
      return 1;
    if (!(myCdr instanceof LispList))
      throw new WrongTypeArgumentException("listp", myCdr);
    return 1 + ((LispList) myCdr).size();
  }

  public void setCdr (@Nullable LispObject cdr) {
    if (Predicate.isNil(cdr)) {
      myCdr = null;
      isTrueList = true;
      return;
    }
    myCdr = cdr;
    isTrueList = cdr instanceof LispList && ((LispList) cdr).isTrueList();
  }

  public LispList delq (LispObject element) {
    if (isEmpty())
      return this;
    LispList list = this;
    LispObject tail = this;
    LispList prev = LispList.list();
    while (tail instanceof LispList) {
      LispObject cdr = ((LispList) tail).realCdr();
      if (Core.eqs(element, ((LispList) tail).realCar())) {
        if (prev.isEmpty()) {
          if (cdr == null)
            list = LispList.list();
          else if (cdr instanceof LispList)
            list = (LispList) cdr;
          else
            throw new WrongTypeArgumentException("listp", tail);
        }
        else
          prev.setCdr(cdr);
      } else
        prev = (LispList) tail;
      tail = cdr;
    }
    if (tail != null) {
      throw new WrongTypeArgumentException("listp", this);
    }
    return list;
  }

  public LispObject assq (LispObject first) {
    for (LispObject item: toLispObjectList()) {
      if (item instanceof LispList) {
        if (Core.eqs(first, ((LispList) item).realCar()))
          return item;
      }
    }
    return LispSymbol.ourNil;
  }

  public LispObject nthCdr (int n) {
    LispObject result = this;
    int i = 0;
    for (; i < n && result instanceof LispList; ++i) {
      result = ((LispList)result).realCdr();
    }
    if (i < n && result != null)
      throw new WrongTypeArgumentException("listp", result);
    if (result == null)
      return LispSymbol.ourNil;
    return result;
  }

  @Override
  public LispList copy() {
    return new LispList(toLispObjectList());
  }

  public void addToBeginning (LispObject object) {
    if (myCar == null) {
      myCar = object;
      return;
    }
    myCdr = this;
    myCar = object;
  }

  public void setCar (@NotNull LispObject car) {
    myCar = car;
  }

  /**
   * current list values are reset with values from given list. This list is enlarged if needed.
   * If some list.length() < length(), the odd cells are set to nil.
   * @param list the list to reset with
   */
  public void resetWith (LispList list) {
    LispObject tail = list;
    LispObject current = this;
    while (tail instanceof LispList) {
      if (((LispList)current).realCar() == null) {
        if (((LispList)tail).realCar() != null)
          ((LispList)current).setCar(((LispList)tail).realCar());
      }
      else ((LispList)current).setCar(((LispList)tail).car());

      if (!(((LispList)current).realCdr() instanceof LispList)) {
        ((LispList)current).setCdr(((LispList)tail).realCdr());
        return;
      }

      tail = ((LispList)tail).realCdr();

      current = ((LispList)current).realCdr();

      if (tail instanceof LispList)
        continue;

      ((LispList)current).setCar(Core.thisOrNil(tail));
      while (((LispList)current).realCdr() instanceof LispList) {

        ((LispList)current).setCar(LispSymbol.ourNil);
        current = ((LispList)current).realCdr();
      }
      ((LispList)current).setCar(LispSymbol.ourNil);
      ((LispList)current).setCdr(LispSymbol.ourNil);
    }
  }

  public LispList sort (final LispObject predicate) {
    LispList sorted = unchangingSort(predicate);
    myCdr = null;
    return sorted;
  }

  private LispList unchangingSort (final LispObject predicate) {
    int size = size();
    if (size < 2)
      return this;
    List<LispObject> left = new ArrayList<>();
    int middle = size / 2;
    LispList list = this;
    for (int i = 0; i < middle; i++, list = list.getListCdr()) {
      left.add(list.car());
    }
    List<LispObject> right = new ArrayList<>();
    for (int i = middle; i < size; i++, list = list.getListCdr()) {
      right.add(list.car());
    }
    return merge(LispList.list(left).unchangingSort(predicate), LispList.list(right).unchangingSort(predicate), predicate);
  }

  private static LispList merge (LispList left, LispList right, final LispObject predicate) {
    List<LispObject> list = new ArrayList<>();
    while (left != null && right != null) {
      LispObject predicateValue = Core.functionCall(GlobalEnvironment.INSTANCE, predicate, left.car(), right.car());
      if (predicateValue.equals(LispSymbol.ourNil)) {
        list.add(right.car());
        right = right.getListCdr();
      } else {
        list.add(left.car());
        left = left.getListCdr();
      }
    }
    LispList merged = LispList.list(list);
    if (left != null) {
      merged.append(left);
    }
    if (right != null) {
      merged.append(right);
    }
    return merged;
  }

  private LispList getListCdr () {
    if (myCdr == null || myCdr instanceof LispList)
      return (LispList) myCdr;
    throw new WrongTypeArgumentException("listp", myCdr);
  }
}