package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
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
public class LispList implements LispObject, LispSequence, LispKeymap {
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

    public static LispList cons (LispObject car, LispObject cdr) {
        return new LispList(car, cdr);
    }

    private LispList (List<LispObject> data, boolean test) {
        if (data == null || data.size() == 0) {
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

    private LispList (List<LispObject> data) {
        isTrueList = true;
        if (data == null || data.size() == 0) {
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

    private LispList (@NotNull LispObject car, @NotNull LispObject cdr) {
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
    public LispObject evaluate(Environment environment) {
        if (isEmpty())
            return LispSymbol.ourNil;
        //todo: if assotiated list?

        LispObject function = car();
        if (function instanceof LispSymbol) {
            LispSymbol fun = (LispSymbol) function;
            LispSymbol symbol = GlobalEnvironment.INSTANCE.find(fun.getName());
            if (symbol == null || !symbol.isFunction()) {
                //while we are not loading all elisp code, perform search on request
                System.out.println("FUN " + fun.getName());
                try {
                    symbol = GlobalEnvironment.INSTANCE.findAndRegisterEmacsForm(fun, GlobalEnvironment.SymbolType.FUN);
                } catch (RuntimeException e) {
                    System.err.println(e.getMessage());
                    throw new VoidFunctionException(fun.getName());
                }
                if (symbol == null || !symbol.isFunction())
                    throw new VoidFunctionException(fun.getName());
            }
            List<LispObject> data = myCdr instanceof LispList ? ((LispList)myCdr).toLispObjectList() : new ArrayList<LispObject>();
            return symbol.evaluateFunction(environment, data);
        } else if (function instanceof LispList) {
            Lambda lambda = new Lambda((LispList) function);
            List<LispObject> args = myCdr instanceof LispList ? ((LispList)myCdr).toLispObjectList() : new ArrayList<LispObject>();
            if (!environment.areArgumentsEvaluated()) {
                for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                    args.set(i, args.get(i).evaluate(environment));
                }
            } else {
                environment.setArgumentsEvaluated(false);
            }
            return lambda.evaluate(environment, args);
        }
        throw new InvalidFunctionException(function.toString());        
    }

    @Override
    public List<LispObject> toLispObjectList() {
        ArrayList<LispObject> list = new ArrayList<>();
        if (isEmpty())
            return list;
        LispObject cell = this;
        do {
            LispObject cdr = ((LispList)cell).cdr();
            if (cdr == null || cdr instanceof LispList || cdr.equals(LispSymbol.ourNil) || ((LispList)cell).isTrueList()) {
                if (((LispList)cell).car() != null)
                    list.add(((LispList)cell).car());
            } else {
                    list.add(cell);
                    break;
                }
            cell = cdr;
        } while (cell != null && cell instanceof LispList);

        return list;
    }

    @Override
    public List<LispObject> mapCar(Environment environment, LispObject method) {
        LispObject list = this;
        ArrayList<LispObject> data = new ArrayList<>();
        while (list instanceof LispList) {
            data.add(BuiltinsCore.functionCall(environment, method, ((LispList) list).car()));
            list = ((LispList) list).cdr();
        }
        if (list.equals(LispSymbol.ourNil)) {
            return data;
        }
        throw new WrongTypeArgumentException("listp", list);
    }

    @Override
    public String toCharString() {
        List<LispObject> list = toLispObjectList();
        String s = "";
        for (LispObject element: list) {
            if (!BuiltinPredicates.isCharacter(element))
                throw new WrongTypeArgumentException("characterp", element);
            s += ((LispInteger)element).toCharacterString();
        }
        return s;
    }

    private boolean isNil (LispObject object) {
        return (object instanceof LispSymbol && ((LispSymbol) object).getName().equals("nil"));
    }

    @Override
    public String toString() {
        return toString(true);
    }

    private String toString (boolean drawBrackets) {
        List<LispObject> objectList = toLispObjectList();
        if (objectList.isEmpty())
            return "nil";
//        if (objectList.size() == 1 && objectList.get(0).equals(LispSymbol.ourNil))
//            return "nil";
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

    public LispObject car () {
        return myCar == null ? LispSymbol.ourNil : myCar;
    }

    public LispObject cdr () {
        return myCdr == null ? LispSymbol.ourNil : myCdr;
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

    public LispList memq (LispObject element, String equalityFunctionName) {
        LispObject cdr = this;
        try {
            if (equalityFunctionName.equals("eq")) {
                for (; cdr != LispSymbol.ourNil; cdr = ((LispList)cdr).cdr()) {
                    if (BuiltinsCore.eqs(((LispList) cdr).car(), element)) {
                        return (LispList)cdr;
                    }
                }
                return LispList.list();
            }

            if (equalityFunctionName.equals("equal")) {
                for (; cdr != LispSymbol.ourNil; cdr = ((LispList)cdr).cdr()) {
                    if (BuiltinsCore.equals(((LispList) cdr).car(), element)) {
                        return (LispList)cdr;
                    }
                }
                return LispList.list();
            }
            throw new RuntimeException("Wrong usage!");
        } catch (ClassCastException e) {
            throw new WrongTypeArgumentException("listp", cdr);
        }
    }

    public void setCdr (LispObject cdr) {
        myCdr = cdr;
    }

    public LispObject nReverse () {
        if (isEmpty())
            return this;
        LispObject prev = LispSymbol.ourNil;
        LispObject tail = this;
        while (tail instanceof LispList) {
            LispObject next = ((LispList)tail).cdr();
            ((LispList) tail).setCdr(prev);
            prev = tail;
            tail = next;
        }
        if (!tail.equals(LispSymbol.ourNil)) {
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
            if (myCdr == null || myCdr.equals(LispSymbol.ourNil)) {
                myCdr = object;
                if (!(object instanceof LispList))
                    isTrueList = false;
            } else
                throw new WrongTypeArgumentException("listp", myCdr);
        }
    }

    @Override
    public int length() {
        return toLispObjectList().size();
    }

    public void resetTail (LispObject newTail) {
        if (myCdr instanceof LispList) {
            ((LispList) myCdr).resetTail(newTail);
            return;
        }
        if (myCdr == null || myCdr.equals(LispSymbol.ourNil)) {
            myCdr = newTail;
            isTrueList = false;
        }
    }

    public LispObject tail() {
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

    public LispList delq (LispObject element) {
        if (isEmpty())
            return this;
        LispList list = this;
        LispObject tail = this;
        LispList prev = LispList.list();
        while (tail instanceof LispList) {
            if (BuiltinsCore.eqs(element, ((LispList) tail).car())) {
                if (prev.isEmpty()) {
                    if (((LispList) tail).cdr().equals(LispSymbol.ourNil))
                        list = LispList.list();
                    else if (((LispList) tail).cdr() instanceof LispList)
                        list = (LispList) ((LispList) tail).cdr();
                    else
                        throw new WrongTypeArgumentException("listp", tail);
                }
                else
                    prev.setCdr(((LispList) tail).cdr());
            } else
                prev = (LispList) tail;
            tail = ((LispList) tail).cdr();
        }
        if (!tail.equals(LispSymbol.ourNil)) {
            throw new WrongTypeArgumentException("listp", this);
        }
        return list;
    }
    
    public LispObject assq (LispObject first) {
        for (LispObject item: toLispObjectList()) {
            if (item instanceof LispList) {
                if (BuiltinsCore.eqs(first, ((LispList) item).car()))
                    return item;
            }
        }
        return LispSymbol.ourNil;
    }

    //========= as keymap =============================


    @Override
    public void defineKey(Environment environment, LispObject function, LispStringOrVector key) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispObject accessKeymap(LispObject idx, boolean tOk, boolean noInherit) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispSymbol getKeyDefinition(Shortcut key) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispObject getKeyDefinition(LispStringOrVector key, @Nullable @Optional LispObject acceptDefault) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispKeymap getParent() {
        LispObject parent = tail();
        return parent.equals(LispSymbol.ourNil) || (!parent.equals(LispSymbol.ourNil) && !(parent instanceof LispKeymap))
                ? null
                : (LispKeymap) parent;
    }

    @Override
    public void setParent(@Nullable LispKeymap parent) {
        //todo: If keymap has submaps (bindings for prefix keys), they too receive new parent keymaps
        // that reflect what parent specifies for those prefix keys.
        resetTail(parent);
    }

    @Override
    public LispKeymap copy() {
        return new LispList(toLispObjectList());
    }

    @Override
    public boolean keymapMemberP(LispObject maps) {
        return false;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
