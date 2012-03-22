package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey;

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
public class LispList extends LispKeymapImpl implements LispSequence {
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
            throw new InternalError("Null element in LispList!");
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
            throw new InternalError("Null element in LispList!");
        }
        myCar = data.get(0);
        if (data.size() == 1) {
            return;
        }
        myCdr = new LispList(data.subList(1, data.size()));
    }

    private LispList (@NotNull LispObject car, @Nullable LispObject cdr) {
        if (BuiltinPredicates.isNil(cdr)) {
            myCar = car;
            isTrueList = true;
            return;
        }
        myCar = car;
        myCdr = cdr;
        isTrueList = cdr instanceof LispList && ((LispList)cdr).isTrueList();
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
            data.add(BuiltinsCore.functionCall(environment, method, ((LispList) list).car()));
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
            if (!BuiltinPredicates.isCharacter(element))
                throw new WrongTypeArgumentException("characterp", element);
            s += ((LispInteger)element).toCharacterString();
        }
        return s;
    }

    @Override
    public String toString() {
        return toString(true);
    }

    private String toString (boolean drawBrackets) {
        List<LispObject> objectList = toLispObjectList();
        if (objectList.isEmpty())
            return "nil";
        String list = drawBrackets ? "(" : "";
        String closingBracket = drawBrackets ? ")" : "";
        if (isTrueList) {
            for (int i = 0, objectListSize = objectList.size(); i < objectListSize; i++) {
                LispObject item = objectList.get(i);
//                if (i == objectListSize - 1 && item instanceof LispList && !((LispList)item).isTrueList())
//                    list += ((LispList)item).toString(false) + " ";
//                else
                    list += item.toString() + " ";
            }
            return list.trim() + closingBracket;
        } else {
            if (myCdr == null)
                list += myCar.toString() + closingBracket;
            else {
                list += myCar.toString();
                if (myCdr instanceof LispList)
                    list += " " + ((LispList)myCdr).toString(false) + closingBracket;
                else
                    list += " . " + myCdr.toString() + closingBracket;
            }
            return list;
        }
    }

    public LispObject car () {
        return BuiltinsCore.thisOrNil(myCar);
    }

    public LispObject cdr () {
        return BuiltinsCore.thisOrNil(myCdr);
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
                for (; cdr != null; cdr = ((LispList)cdr).realCdr()) {
                    if (((LispList) cdr).realCar() != null && BuiltinsCore.eqs(element, ((LispList) cdr).realCar())) {
                        return (LispList)cdr;
                    }
                }
                return LispList.list();
            }

            if (equalityFunctionName.equals("equal")) {
                for (; cdr != null; cdr = ((LispList)cdr).realCdr()) {
                    if (((LispList) cdr).realCar() != null && BuiltinsCore.equals(element, ((LispList) cdr).realCar())) {
                        return (LispList)cdr;
                    }
                }
                return LispList.list();
            }
            throw new InternalError("Wrong usage of LispList.memq()!");
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
    public int length() {
        return toLispObjectList().size();
    }

    public void setCdr (@Nullable LispObject cdr) {
        if (BuiltinPredicates.isNil(cdr)) {
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
            if (BuiltinsCore.eqs(element, ((LispList) tail).realCar())) {
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
                if (BuiltinsCore.eqs(first, ((LispList) item).realCar()))
                    return item;
            }
        }
        return LispSymbol.ourNil;
    }

    //========= as keymap =============================


    @Override
    public LispSymbol getKeyDefinition(Shortcut key) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispObject getKeyDefinition(LispStringOrVector key, @Nullable @Optional LispObject acceptDefault) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    public LispObject nthCdr (int n) {
        LispObject result = this;
        int i = 0;
        for (; i < n && result instanceof LispList; ++i) {
            result = ((LispList)result).realCdr();
        }
        if (i < n && result != null)
            throw new WrongTypeArgumentException("listp", result);
        return result;
    }

    @Override
    public LispKeymap getParent() {
        List<LispObject> data = toLispObjectList();
        int parentIndex = data.subList(1, data.size()).indexOf(BuiltinsKey.ourKeyMapSymbol) + 1;
        if (parentIndex == 0)
            return null;
        LispObject parent = nthCdr(parentIndex);
        return parent == null ? null : (LispKeymap)parent;
    }

    private void setKeymapParent (@Nullable LispKeymap parent) {
        if ((myCdr != null && ((LispKeymapImpl)myCdr).isParentOf(parent))
            || (myCdr == null && isParentOf(parent)))
            BuiltinsCore.error("Cyclic keymap inheritance");
        setCdr(parent);
    }

    @Override
    public void setParent(@Nullable LispKeymap parent) {
        //todo: If keymap has submaps (bindings for prefix keys), they too receive new parent keymaps
        if (BuiltinsKey.isListKeymap(myCdr)) { //this is the current parent
            setKeymapParent(parent);
            return;
        }
        if (myCdr != null && myCdr instanceof LispList && isTrueList)
            ((LispList) myCdr).setParent(parent);
        else setKeymapParent(parent);
    }

    @Override
    public LispKeymap copy() {
        return new LispList(toLispObjectList());
    }
}
