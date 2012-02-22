package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

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
public class LispList extends LispObject implements LispSequence {
    private LObject myCar = null;
    private LObject myCdr = null;
    private Boolean isTrueList;

    public static LispList list (LObject ... objects) {
        return new LispList(new ArrayList<LObject>(Arrays.asList(objects)));
    }

    public static LispList list (List<LObject> data) {
        return new LispList(data);
    }

    public static LispList testList (LObject ... objects) {
        return new LispList(new ArrayList<LObject>(Arrays.asList(objects)), true);
    }

    public static LispList cons (LObject car, LObject cdr) {
        return new LispList(car, cdr);
    }

    private LispList (List<LObject> data, boolean test) {
        if (data == null || data.size() == 0) {
//            isTrueList = true;
//            myCar = LispSymbol.ourNil;
//            myCdr = LispSymbol.ourNil;
            return;
        }

        if (data.get(0) == null) {
            throw new RuntimeException("Null element in LispList!");
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

    private LispList (List<LObject> data) {
        isTrueList = true;
        if (data == null || data.size() == 0) {
//            myCar = LispSymbol.ourNil;
//            myCdr = LispSymbol.ourNil;
            return;
        }

        if (data.get(0) == null) {
            throw new RuntimeException("Null element in LispList!");
        }

        myCar = data.get(0);
        if (data.size() == 1) {
            myCdr = LispSymbol.ourNil;
            return;
        }
        myCdr = new LispList(data.subList(1, data.size()));
    }

    private LispList (@NotNull LObject car, @NotNull LObject cdr) {
        myCar = car;
        myCdr = cdr;
        isTrueList = false;
    }

    public boolean isTrueList() {
        return isTrueList;
    }

    public boolean isEmpty() {
        return (myCar == null && myCdr == null);
    }

    /**
     * @param environment@return the result of last function execution
     */
    @Override
    public LObject evaluate(Environment environment) {
        if (isEmpty())
            return LispSymbol.ourNil;
        //todo: if assotiated list?

        LispSymbol fun;
        try {
            fun = (LispSymbol)car();
        } catch (ClassCastException e) {
            throw new InvalidFunctionException(car().toString());
        }
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(fun.getName());
        if (symbol == null || !symbol.isFunction()) {
            //while we are not loading all elisp code, perform search on request
            System.out.println("FUN " + fun.getName());
            try {
//                if (fun.getName().equals("cl-block-wrapper"))
//                    System.out.print(1);
                symbol = GlobalEnvironment.INSTANCE.findAndRegisterEmacsForm(fun);
            } catch (RuntimeException e) {
                System.err.println(e.getMessage());
                throw new VoidFunctionException(fun.getName());
            }
            if (symbol == null || !symbol.isFunction())
                throw new VoidFunctionException(fun.getName());
            //System.out.println("uploaded " + fun.getName());
        }
        /*if (symbol.getName().equals("append") || symbol.getName().equals("backquote-process"))
            System.out.print(1);*/
        List<LObject> data = myCdr instanceof LispList ? ((LispList)myCdr).toLObjectList() : new ArrayList<LObject>();
        return symbol.evaluateFunction(environment, data);
    }

    @Override
    public List<LObject> toLObjectList() {
        ArrayList<LObject> list = new ArrayList<>();
        if (isEmpty())
            return list;
        LObject cell = this;
        do {
            LObject cdr = ((LispList)cell).cdr();
            if (cdr == null || cdr instanceof LispList || cdr.equals(LispSymbol.ourNil) || ((LispList)cell).isTrueList())
                if (((LispList)cell).car() != null)
                    list.add(((LispList)cell).car());
                else {
                    list.add(cell);
                    break;
                }
            cell = cdr;
        } while (cell != null && cell instanceof LispList);

        return list;
    }

    @Override
    public List<LObject> mapCar(Environment environment, LispSymbol method) {
        LObject list = this;
        ArrayList<LObject> data = new ArrayList<>();
        while (list instanceof LispList) {
            data.add(BuiltinsCore.functionCall(environment, method, ((LispList) list).car()));
            list = ((LispList) list).cdr();
        }
        if (list.equals(LispSymbol.ourNil)) {
            return data;
        }
        throw new WrongTypeArgumentException("listp", list.toString());
    }

    @Override
    public LObject copy() {
        return new LispList(toLObjectList());
    }

    private boolean isNil (LObject object) {
        return (object instanceof LispSymbol && ((LispSymbol) object).getName().equals("nil"));
    }

    @Override
    public String toString() {
        return toString(true);
    }

    private String toString (boolean drawBrackets) {
        List<LObject> objectList = toLObjectList();
        if (objectList.isEmpty())
            return "nil";
        if (objectList.size() == 1 && objectList.get(0).equals(LispSymbol.ourNil))
            return "nil";
        String list = drawBrackets ? "(" : "";
        if (isTrueList) {
            for (int i = 0; i != objectList.size(); ++i) {
                list += objectList.get(i).toString() + " ";
            }
            return list.trim() + (drawBrackets ? ")" : "");
        } else {
            if (isNil(myCar) && isNil(myCdr))
                list += "nil" + (drawBrackets ? ")" : "");
            else if (isNil(myCdr))
                list += myCar.toString() + (drawBrackets ? ")" : "");
            else {
                list += myCar.toString();
                if (myCdr instanceof LispList)
                    list += " " + ((LispList)myCdr).toString(false) + (drawBrackets ? ")" : "");
                else
                    list += " . " + myCdr.toString() + (drawBrackets ? ")" : "");
            }
            return list;
        }
    }

    public LObject car () {
        return myCar;
    }

    public LObject cdr () {
        return myCdr;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LispList)) {
            return o.equals(LispSymbol.ourNil) && isEmpty();
        }
        LispList lispList = (LispList) o;
        if (myCar != null ? !myCar.equals(lispList.myCar) : lispList.myCar != null) return false;
        if (myCdr != null ? !myCdr.equals(lispList.myCdr) : lispList.myCdr != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myCar != null ? myCar.hashCode() : 0;
        result = 31 * result + (myCdr != null ? myCdr.hashCode() : 0);
        return result;
    }

    public LispObject memq (LObject element, String equalityFunctionName) {
        LObject cdr = this;
        try {
            if (equalityFunctionName.equals("eq")) {
                for (; cdr != LispSymbol.ourNil; cdr = ((LispList)cdr).cdr()) {
                    if (BuiltinsCore.eqs(((LispList) cdr).car(), element)) {
                        return (LispList)cdr;
                    }
                }
                return LispSymbol.ourNil;
            }

            if (equalityFunctionName.equals("equal")) {
                for (; cdr != LispSymbol.ourNil; cdr = ((LispList)cdr).cdr()) {
                    if (BuiltinsCore.equals(((LispList) cdr).car(), element)) {
                        return (LispList)cdr;
                    }
                }
                return LispSymbol.ourNil;
            }
            throw new RuntimeException("Wrong usage!");
        } catch (ClassCastException e) {
            throw new WrongTypeArgumentException("listp", cdr.toString());
        }
    }

    public void setCar (LObject car) {
        myCar = car;
    }

    public void setCdr (LObject cdr) {
        myCdr = cdr;
    }

    public LObject nReverse () {
        if (isEmpty())
            return this;
        LObject prev = LispSymbol.ourNil;
        LObject tail = this;
        while (tail instanceof LispList) {
            LObject next = ((LispList)tail).cdr();
            ((LispList) tail).setCdr(prev);
            prev = tail;
            tail = next;
        }
        if (!tail.equals(LispSymbol.ourNil)) {
            throw new WrongTypeArgumentException("listp", toString());
        }
        return prev;
    }

    public void append (LObject object) {
        if (object.equals(LispSymbol.ourNil))
            return;
        if (myCdr != null && myCdr instanceof LispList)
            ((LispList) myCdr).append(object);
        else {
            if (myCdr == null || myCdr.equals(LispSymbol.ourNil)) {
                myCdr = object;
                if (!(object instanceof LispList))
                    isTrueList = false;
            } else
                throw new WrongTypeArgumentException("listp", myCdr.toString());
        }
    }

    @Override
    public int length() {
        return toLObjectList().size();
    }

    public void resetTail (LObject newTail) {
        if (myCdr instanceof LispList) {
            ((LispList) myCdr).resetTail(newTail);
            return;
        }
        if (myCdr == null || myCdr.equals(LispSymbol.ourNil)) {
            myCdr = newTail;
            isTrueList = false;
        }
    }

    public LObject tail() {
        if (!isTrueList)
            return myCdr;
        if (myCdr instanceof LispList)
            return ((LispList) myCdr).tail();
        if (myCdr.equals(LispSymbol.ourNil))
            return LispSymbol.ourNil;
        if (myCdr == null)
            return myCar;
        throw new LispException("Invalid list: " + toString());
    }

    private LObject del (LObject element) {
        if (myCar.equals(element)) {
            return myCdr;
        }
        return this;
    }

    public LispList delq (LObject element) {
        if (isEmpty())
            return this;
        LispList list = this;
        LObject tail = this;
        LispList prev = LispList.list();
        while (tail instanceof LispList) {
            if (BuiltinsCore.eqs(element, ((LispList) tail).car())) {
                if (prev.isEmpty()) {
                    if (((LispList) tail).cdr().equals(LispSymbol.ourNil))
                        list = LispList.list();
                    else if (((LispList) tail).cdr() instanceof LispList)
                        list = (LispList) ((LispList) tail).cdr();
                    else
                        throw new WrongTypeArgumentException("listp", tail.toString());
                }
                else
                    prev.setCdr(((LispList) tail).cdr());
            } else
                prev = (LispList) tail;
            tail = ((LispList) tail).cdr();
        }
        if (!tail.equals(LispSymbol.ourNil)) {
            throw new WrongTypeArgumentException("listp", toString());
        }
        return list;
    }

}
