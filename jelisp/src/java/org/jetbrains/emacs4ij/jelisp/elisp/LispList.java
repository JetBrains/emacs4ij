package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeyBoardModifier;
import org.jetbrains.emacs4ij.jelisp.KeyBoardUtil;
import org.jetbrains.emacs4ij.jelisp.exception.InternalError;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import org.jetbrains.emacs4ij.jelisp.subroutine.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey.*;

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
        int length = key.length();
        if (length == 0)
            return;
        int meta_bit = (key instanceof LispVector || (key instanceof LispString && ((LispString) key).isMultibyte())
                ? KeyBoardModifier.metaValue() : 0x80);

        if (function instanceof LispVector
                && !((LispVector) function).isEmpty()
                &&  ((LispVector) function).get(0) instanceof LispList)
        { /* function is apparently an XEmacs-style keyboard macro.  */
            int i = ((LispVector) function).length();
            LispVector tmp = LispVector.make(i, LispSymbol.ourNil);
            while (--i >= 0) {
                LispObject c = ((LispVector) function).getItem(i);
                if (c instanceof LispList && lucidEventTypeListP(c))
                    c = eventConvertList(c);
                tmp.setItem(i, c);
            }
            function = tmp;
        }

        int idx = 0;
        boolean metized = false;
        while (true) {
            LispObject c = BuiltinsCore.aRef((LispArray) key, new LispInteger(idx));
            if (c instanceof LispList) {
                if (lucidEventTypeListP(c))
                    c = eventConvertList(c);
                else if (BuiltinPredicates.isCharacter(((LispList) c).car())) {
                    if (!BuiltinPredicates.isCharacter(((LispList) c).cdr()))
                        throw new WrongTypeArgumentException("characterp", ((LispList) c).cdr());
                }
            }
            if (c instanceof LispSymbol)
                sillyEventSymbolError(c);

            if (c instanceof LispInteger && ((((LispInteger) c).getData() & meta_bit) != 0) && !metized) {
                c = GlobalEnvironment.INSTANCE.find("meta-prefix-char").getValue();
                metized = true;
            }
            else {
                if (c instanceof LispInteger)
                    ((LispInteger) c).setData(((LispInteger) c).getData() & ~meta_bit);
                metized = false;
                idx++;
            }

            if (!(c instanceof LispInteger) && !(c instanceof LispSymbol)
                    && (!(c instanceof LispList)
                    /* If C is a range, it must be a leaf.  */
                    || (((LispList) c).car() instanceof LispInteger && idx != length)))
                BuiltinsCore.error("Key sequence contains invalid event");

            if (idx == length) {
                store(c, function);
                return;
            }

            LispObject cmd = accessKeymap(c, false, true);

            /* If this key is undefined, make it a prefix.  */
            if (cmd.equals(LispSymbol.ourNil))
                cmd = defineAsPrefix(c);

            LispObject keymap = getKeymap(cmd);
            if (keymap == null)
                BuiltinsCore.error(String.format("Key sequence %s starts with non-prefix key %s",
                        keyDescription(environment, key, LispSymbol.ourNil).getData(),
                        keyDescription(environment, BuiltinsCore.substring(key, new LispInteger(0),
                                new LispInteger(idx)),
                                LispSymbol.ourNil).getData()));
        }
    }


    @Override
    public LispObject accessKeymap(LispObject idx, boolean tOk, boolean noInherit) {
        LispSymbol unbound = new LispSymbol("unbound");
        LispObject val = unbound;
        idx = BuiltinPredicates.eventHead(idx);

        /* If idx is a symbol, it might have modifiers, which need to be put in the canonical order.  */
        if (idx instanceof LispSymbol)
            idx = KeyBoardUtil.reorderModifiers(idx);
        else if (idx instanceof LispInteger)
            ((LispInteger) idx).setData(((LispInteger) idx).getData() & (CharUtil.CHAR_META | (CharUtil.CHAR_META - 1)));

        LispList map = this;

        /* Handle the special meta -> esc mapping. */
        if (idx instanceof LispInteger && KeyBoardModifier.META.bitwiseAndNotZero((LispInteger) idx)) {
            /* See if there is a meta-map.  If there's none, there is
no binding for IDX, unless a default binding exists in MAP.  */
            /* A strange value in which Meta is set would cause
     infinite recursion.  Protect against that.  */
            LispInteger metaPrefixChar = (LispInteger)GlobalEnvironment.INSTANCE.find("meta-prefix-char").getValue();

            if ((metaPrefixChar.getData() & CharUtil.CHAR_META) != 0)
                metaPrefixChar.setData(27);
            LispObject metaMap = accessKeymap(metaPrefixChar, tOk, noInherit);
            if (metaMap instanceof LispList) {
                map = (LispList) metaMap;
                ((LispInteger) idx).setData(KeyBoardModifier.META.bitwiseAndNot((LispInteger) idx));
            }
            else if (tOk)
                /* Set IDX to t, so that we only find a default binding.  */
                idx = LispSymbol.ourT;
            else
                /* We know there is no binding.  */
                return LispSymbol.ourNil;
        }

        /* t_binding is where we put a default binding that applies, to use in case we do not find a binding specifically
            for this key sequence.  */

        LispObject t_binding = LispSymbol.ourNil;
        for (LispObject tail = map.cdr();
             tail instanceof LispList || ((tail = getKeymap (tail)) != null);
             tail = ((LispList)tail).cdr())
        {
            LispObject binding = ((LispList)tail).car();
            if (binding instanceof LispSymbol) {
                /* If NOINHERIT, stop finding prefix definitions after we pass a second occurrence of the `keymap' symbol.  */
                if (noInherit && BuiltinsCore.eqs(binding, BuiltinsKey.ourKeyMapSymbol))
                    return null;
            }
            else if (binding instanceof LispList) {
                LispObject key = ((LispList) binding).car();
                if (BuiltinsCore.eqs(key, idx))
                    val = ((LispList) binding).cdr();
                else if (tOk && BuiltinsCore.eqs(key, LispSymbol.ourT)) {
                    t_binding = ((LispList) binding).cdr();
                    tOk = false;
                }
            }
            else if (binding instanceof LispVector) {
                if (BuiltinPredicates.isWholeNumber(idx) && ((LispInteger)idx).getData() < ((LispVector) binding).length())
                    val = ((LispVector) binding).getItem(((LispInteger)idx).getData());
            }
            else if (binding instanceof LispCharTable) {
                /* Character codes with modifiers
        are not included in a char-table.
        All character codes without modifiers are included.  */
                if (BuiltinPredicates.isWholeNumber(idx) && (((LispInteger)idx).getData() & CharUtil.CHAR_MODIFIER_MASK) == 0) {
                    val = ((LispCharTable) binding).getItem(((LispInteger) idx).getData());
                    /* `nil' has a special meaning for char-tables, so
                 we use something else to record an explicitly
                 unbound entry.  */
                    if (val.equals(LispSymbol.ourNil))
                        val = unbound;
                }
            }

            /* If we found a binding, clean it up and return it.  */
            if (!BuiltinsCore.eqs(val, unbound)) {
                if (BuiltinsCore.eqs (val, LispSymbol.ourT))
                    /* A t binding is just like an explicit nil binding (i.e. it shadows any parent binding but not bindings in
               keymaps of lower precedence).  */
                    val = LispSymbol.ourNil;
                val = getKeyElement (val);
                if (isKeymap(val)) {
                    map.fixSubmapInheritance(idx, val);
                }
                return val;
            }
        }
        return getKeyElement(t_binding);
    }

    @Override
    public LispSymbol getKeyDefinition(Shortcut key) {
        LispObject definition = getKeyDefinition(new LispVector(key), null);
        return definition instanceof LispSymbol ? (LispSymbol) definition : LispSymbol.ourNil;
    }

    @Override
    public LispObject getKeyDefinition(LispStringOrVector key, @Nullable @Optional LispObject acceptDefault) {
        LispKeymap keymap = this;
        int idx = 0;
        int length = key.length();
        while (idx < length) {
            LispObject c = BuiltinsCore.aRef((LispArray) key, new LispInteger(idx++));
            if (c instanceof LispList && lucidEventTypeListP(c))
                c = eventConvertList(c);
            /* Turn the 8th bit of string chars into a meta modifier.  */
            if (c instanceof LispInteger) {
                int cValue = ((LispInteger) c).getData();
                if (key instanceof LispString && (cValue & 0x80) != 0 && !((LispString) key).isMultibyte())
                    ((LispInteger) c).setData((cValue | KeyBoardModifier.metaValue()) & ~0x80);
            }
            if (!(c instanceof LispInteger) && !(c instanceof LispSymbol) && !(c instanceof LispList) && !(c instanceof LispString))
                BuiltinsCore.error("Key sequence contains invalid event");
            LispObject cmd = keymap.accessKeymap(c, !isNil(acceptDefault), false);
            if (idx == length)
                return cmd;
            keymap = getKeymap(cmd);
            if (keymap == null)
                return new LispInteger(idx);
        }
        return LispSymbol.ourNil;
    }

    @Override
    public LispKeymap getParent() {
        LispObject tail = tail();
        if (tail.equals(LispSymbol.ourNil))
            return null;
        try {
            return (LispKeymap)tail;
        } catch (ClassCastException e) {
            throw new InternalError("Invalid emacs keymap!");
        }
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

    private LispObject defineAsPrefix(LispObject c) {
        LispObject cmd = makeSparseKeymap(GlobalEnvironment.INSTANCE, null);
        cmd = BuiltinsList.nConcatenate(cmd, accessKeymap(c, false, false));
        return store(c, cmd);
    }

    private LispObject store (LispObject idx, LispObject def) {
        if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
            if (!BuiltinPredicates.isCharacter(((LispList) idx).cdr()))
                throw new WrongTypeArgumentException("characterp", ((LispList) idx).cdr());
        } else
            idx = BuiltinPredicates.eventHead(idx);

        if (idx instanceof LispSymbol)
            idx = KeyBoardUtil.reorderModifiers(idx);
        else if (idx instanceof LispInteger)
            ((LispInteger) idx).setData(((LispInteger) idx).getData() & (CharUtil.CHAR_META | (CharUtil.CHAR_META - 1)));

        /* Scan the keymap for a binding of idx.  */
        LispList insertionPoint = this;
        for (LispObject tail = cdr(); tail instanceof LispList; tail = ((LispList) tail).cdr())
        {
            LispObject elt = ((LispList) tail).car();
            if (elt instanceof LispVector) {
                if (BuiltinPredicates.isWholeNumber(idx) && ((LispInteger)idx).getData() < ((LispVector) elt).length()) {
                    BuiltinsCore.aSet((LispVector) elt, (LispInteger) idx, def);
                    //todo: set action binding
                    return def;
                }
                else if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    int from = ((LispInteger)((LispList) idx).car()).getData();
                    int to = ((LispInteger)((LispList) idx).cdr()).getData();
                    if (to >= ((LispVector) elt).length())
                        to = ((LispVector) elt).length() - 1;
                    for (; from <= to; from++) {
                        BuiltinsCore.aSet((LispVector) elt, new LispInteger(from), def);
                        //todo: set action binding
                    }
                    if (to == ((LispInteger)((LispList) idx).cdr()).getData())
                        return def;
                }
                insertionPoint = (LispList) tail;
            }
            else if (elt instanceof LispCharTable) {
                if (BuiltinPredicates.isWholeNumber(idx) && (((LispInteger)idx).getData() & CharUtil.CHAR_MODIFIER_MASK) == 0) {
                    BuiltinsCore.aSet((LispArray)elt, (LispInteger) idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                    //todo: set action binding
                    return def;
                }
                else if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    BuiltinsCharTable.setCharTableRange((LispCharTable) elt, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                    //todo: set action bindings
                    return def;
                }
                insertionPoint = (LispList) tail;
            }
            else if (elt instanceof LispList) {
                if (BuiltinsCore.eqs(idx, ((LispList) elt).car())) {
                    ((LispList) elt).setCdr(def);
                    //todo: set action binding
                    return def;
                }
                else if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    int from = ((LispInteger)((LispList) idx).car()).getData();
                    int to = ((LispInteger)((LispList) idx).cdr()).getData();

                    if (from <= ((LispInteger)((LispList) idx).car()).getData()
                            && to >= ((LispInteger)((LispList) idx).car()).getData())
                    {
                        ((LispList) elt).setCdr(def);
                        //todo: set action binding
                        if (from == to)
                            return def;
                    }
                }
            }
            else if (BuiltinsCore.eqs(elt, BuiltinsKey.ourKeyMapSymbol)) {
                LispObject tmp;
                if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    tmp = BuiltinsCharTable.makeCharTable(BuiltinsKey.ourKeyMapSymbol, LispSymbol.ourNil);
                    BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                    //todo: set action binding
                }
                else
                    tmp = LispList.cons(idx, def);
                insertionPoint.setCdr(LispList.cons(tmp, insertionPoint.cdr()));
                //todo: set action binding
                return def;
            }
        }
        LispObject tmp;
        if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
            tmp = BuiltinsCharTable.makeCharTable(BuiltinsKey.ourKeyMapSymbol, LispSymbol.ourNil);
            BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
            //todo: set action binding
        } else
            tmp = LispList.cons(idx, def);
        insertionPoint.setCdr(LispList.cons(tmp, insertionPoint.cdr()));
        //todo: set action binding
        return def;
    }

    private void fixSubmapInheritance(LispObject event, LispObject submap) {
        /* SUBMAP is a cons that we found as a key binding. Discard the other things found in a menu key binding.  */
        submap = getKeymap(getKeyElement(submap));
        if (submap == null)
            return;
        LispKeymap parent = getParent();
        LispKeymap parentEntry = parent != null
                ? getKeymap(parent.accessKeymap(event, false, false))
                : null;

        /* If MAP's parent has something other than a keymap, our own submap shadows it completely.  */
        if (parentEntry == null)
            return;

        if (!BuiltinsCore.eqs (parentEntry, submap)) {
            LispObject submapParent = submap;
            while (true) {
                LispKeymap tem = getKeymap(keymapParent(submapParent));
                if (tem != null) {
                    if (tem.keymapMemberP(parentEntry))
                        /* Fset_keymap_parent could create a cycle.  */
                        return;
                    submapParent = tem;
                }
                else
                    break;
            }
            setKeymapParent(submapParent, parentEntry);
        }
    }

    @Override
    public boolean keymapMemberP(LispObject maps) {
        if (isEmpty())
            return false;
        while (isKeymap(maps) && !BuiltinsCore.eqs(this, maps)) {
            maps = keymapParent(maps);
        }
        return (BuiltinsCore.eqs(this, maps));
    }

    private static LispObject getKeyElement (LispObject object) {
        while (true) {
            if (!(object instanceof LispList))
                /* This is really the value.  */
                return object;
                /* If the keymap contents looks like (keymap ...) or (lambda ...) then use itself. */
            else if (BuiltinsCore.eqs (((LispList) object).car(), BuiltinsKey.ourKeyMapSymbol) ||
                    BuiltinsCore.eqs(((LispList) object).car(), new LispSymbol("lambda")))
                return object;

                /* If the keymap contents looks like (menu-item name . DEFN)
        or (menu-item name DEFN ...) then use DEFN.
        This is a new format menu item.  */
            else if (BuiltinsCore.eqs (((LispList) object).car(), new LispString("menu-item"))) {
                if (((LispList) object).cdr() instanceof LispList) {
                    object = ((LispList) ((LispList) object).cdr()).cdr();
                    LispObject tmp = object;
                    if (object instanceof LispList)
                        object = ((LispList) object).car();
                    /* If there's a `:filter FILTER', apply FILTER to the menu-item's definition to get the real definition to use.  */
                    for (; tmp instanceof LispList && ((LispList) tmp).cdr() instanceof LispList; tmp = ((LispList) tmp).cdr())
                        if (BuiltinsCore.eqs (((LispList) tmp).car(), new LispString(":filter"))) {
                            LispObject filter = ((LispList) ((LispList) tmp).cdr()).car();
                            filter = LispList.list(filter, LispList.list(new LispSymbol("quote"), object));
                            object = filter.evaluate(GlobalEnvironment.INSTANCE);
                            break;
                        }
                }
                else
                    /* Invalid keymap.  */
                    return object;
            }

            /* If the keymap contents looks like (STRING . DEFN), use DEFN.
     Keymap alist elements like (CHAR MENUSTRING . DEFN) will be used by HierarKey menus.  */
            else if (((LispList) object).car() instanceof LispString) {
                object = ((LispList) object).cdr();
                /* Also remove a menu help string, if any, following the menu item name.  */
                if (object instanceof LispList && ((LispList) object).car() instanceof LispString)
                    object = ((LispList) object).cdr();
                /* Also remove the sublist that caches key equivalences, if any.  */
                if (object instanceof LispList && ((LispList) object).car() instanceof LispList) {
                    LispObject carcar = ((LispList) ((LispList) object).car()).car();
                    if (carcar.equals(LispSymbol.ourNil) || carcar instanceof LispVector)
                        object = ((LispList) object).cdr();
                }
            }

            /* If the contents are (KEYMAP . ELEMENT), go indirect.  */
            else {
                LispKeymap map1 = getKeymap(((LispList) object).car());
                return map1 == null
                        ? object /* Invalid keymap */
                        : map1.accessKeymap(((LispList) object).cdr(), false, false);
            }
        }
    }

    private static void sillyEventSymbolError(LispObject c) {
        LispList parsed = KeyBoardUtil.parseModifiers(c);
        int modifiers = ((LispInteger)((LispList)parsed.cdr()).car()).getData();
        LispSymbol base = (LispSymbol) parsed.car();
        LispList assoc = BuiltinsList.assoc(new LispString(base.getName()), BuiltinsKey.ourExcludeKeys);
        if (!assoc.isEmpty()) {
            String new_mods = KeyBoardModifier.test(modifiers);
            c = KeyBoardUtil.reorderModifiers(c);
            String keyString = new_mods + assoc.cdr().toString();
            if (!(c instanceof LispSymbol))
                throw new WrongTypeArgumentException("silly_event_symbol_error: symbolp ", c);
            BuiltinsCore.error (String.format((KeyBoardModifier.META.bitwiseAndNotNotZero(modifiers)
                    ? "To bind the key %s, use [?%s], not [%s]"
                    : "To bind the key %s, use \"%s\", not [%s]"),
                    ((LispSymbol) c).getName(), keyString,
                    ((LispSymbol) c).getName()));
        }
    }
}
