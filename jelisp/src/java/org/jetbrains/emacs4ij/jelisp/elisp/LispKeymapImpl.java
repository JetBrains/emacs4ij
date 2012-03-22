package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeyBoardModifier;
import org.jetbrains.emacs4ij.jelisp.KeyBoardUtil;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.*;

import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates.lucidEventTypeListP;
import static org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/13/12
 * Time: 3:42 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class LispKeymapImpl implements LispKeymap {
    @Override
    public LispObject defineKey(Environment environment, LispObject function, LispStringOrVector key) {
        int length = key.length();
        if (length == 0)
            return LispSymbol.ourNil;
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
            //todo?
            function = tmp;
        }

        int idx = 0;
        int metized = 0;
        LispKeymapImpl keymap = this;
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
                silly_event_symbol_error (c);

            if (c instanceof LispInteger && ((((LispInteger) c).getData() & meta_bit) != 0) && metized == 0) {
                c = GlobalEnvironment.INSTANCE.find("meta-prefix-char").getValue();
                metized = 1;
            }
            else {
                if (c instanceof LispInteger)
                    ((LispInteger) c).setData(((LispInteger) c).getData() & ~meta_bit);
                metized = 0;
                idx++;
            }

            if (!(c instanceof LispInteger) && !(c instanceof LispSymbol)
                    && (!(c instanceof LispList)
                    /* If C is a range, it must be a leaf.  */
                    || (((LispList) c).car() instanceof LispInteger && idx != length)))
                BuiltinsCore.error("Key sequence contains invalid event");

            if (idx == length)
                return keymap.store(c, function);

            LispObject cmd = accessKeymap(c, false, true);

            /* If this key is undefined, make it a prefix.  */
            if (cmd.equals(LispSymbol.ourNil))
                cmd = keymap.define_as_prefix (c);

            keymap = (LispKeymapImpl) getKeymap(cmd);
            if (keymap == null)
                BuiltinsCore.error(String.format("Key sequence %s starts with non-prefix key %s",
                        BuiltinsKey.keyDescription(environment, key, LispSymbol.ourNil).getData(),
                        BuiltinsKey.keyDescription(environment, BuiltinsCore.substring(key, new LispInteger(0),
                                new LispInteger(idx)),
                                LispSymbol.ourNil).getData()));
        }        
    }

    private LispObject accessKeymap(LispObject idx, boolean tOk, boolean noInherit) {
        LispSymbol unbound = new LispSymbol("unbound");
        LispObject val = unbound;
        idx = BuiltinPredicates.eventHead(idx);
        /* If idx is a symbol, it might have modifiers, which need to be put in the canonical order.  */
        if (idx instanceof LispSymbol)
            idx = KeyBoardUtil.reorderModifiers(idx);
        else if (idx instanceof LispInteger)
            ((LispInteger) idx).setData(((LispInteger) idx).getData() & (CharUtil.CHAR_META | (CharUtil.CHAR_META - 1)));

        LispKeymap map = this;
        /* Handle the special meta -> esc mapping. */
        if (idx instanceof LispInteger && KeyBoardModifier.META.bitwiseAndNotZero((LispInteger) idx)) {
            /* See if there is a meta-map.  If there's none, there is no binding for IDX, unless a default binding exists in MAP.  */
            /* A strange value in which Meta is set would cause infinite recursion.  Protect against that.  */
            LispInteger meta_prefix_char = (LispInteger)GlobalEnvironment.INSTANCE.find("meta-prefix-char").getValue();
            if ((meta_prefix_char.getData() & CharUtil.CHAR_META) != 0)
                meta_prefix_char.setData(27);
            LispObject metaMap = accessKeymap(meta_prefix_char, tOk, noInherit);
            if (metaMap instanceof LispList) {
                map = (LispKeymapImpl) metaMap;
                ((LispInteger) idx).setData(KeyBoardModifier.META.bitwiseAndNot((LispInteger) idx));
            }
            else if (tOk)
                /* Set IDX to t, so that we only find a default binding.  */
                idx = LispSymbol.ourT;
            else
                /* We know there is no binding.  */
                return LispSymbol.ourNil;
        }
        /* trueBinding is where we put a default binding that applies, to use in case we do not find a binding
        specifically for this key sequence.  */
        LispObject trueBinding = LispSymbol.ourNil;
        if (map instanceof LispList)
            for (LispObject tail = ((LispList) map).cdr();
                 tail instanceof LispList || (tail = getKeymap(tail)) != null;
                 tail = ((LispList)tail).cdr())
            {
                LispObject binding = ((LispList)tail).car();
                if (binding instanceof LispSymbol) {
                    /* If NOINHERIT, stop finding prefix definitions after we pass a second occurrence of the `keymap' symbol.  */
                    if (noInherit && BuiltinsCore.eqs(binding, ourKeyMapSymbol))
                        return LispSymbol.ourNil;
                }
                else if (binding instanceof LispList) {
                    LispObject key = ((LispList) binding).car();
                    if (BuiltinsCore.eqs(key, idx))
                        val = ((LispList) binding).cdr();
                    else if (tOk && BuiltinsCore.eqs(key, LispSymbol.ourT)) {
                        trueBinding = ((LispList) binding).cdr();
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
                    if (isKeymap(val))
                        ((LispKeymapImpl)map).fixSubmapInheritance(idx, val);
                    return val;
                }
//            QUIT;
            }
        return getKeyElement(trueBinding);
    }

//    Check whether THIS is one of MAPS parents.
    private boolean keymapMemberP(LispObject maps) {
        if (isEmpty())
            return false;
        while (isKeymap(maps) && !BuiltinsCore.eqs(this, maps)) {
            maps = BuiltinsKey.keymapParent(maps);
        }
        return (BuiltinsCore.eqs(this, maps));
    }

    private LispObject define_as_prefix (LispObject c) {
        LispObject cmd = BuiltinsKey.makeSparseKeymap(null);
        cmd = BuiltinsList.nConcatenate(cmd, accessKeymap(c, false, false));
        store(c, cmd);
        return cmd;
    }

    private LispObject store (LispObject idx, LispObject def) {
//        if (!isListKeymap(keymap))
//            BuiltinsCore.error("attempt to define a key in a non-keymap");
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

        LispList insertionPoint = (LispList) this;
        for (LispObject tail = ((LispList)this).cdr(); tail instanceof LispList; tail = ((LispList) tail).cdr()) {
            LispObject elt = ((LispList) tail).car();
            if (elt instanceof LispVector) {
                if (BuiltinPredicates.isWholeNumber(idx) && ((LispInteger)idx).getData() < ((LispVector) elt).length()) {
                    BuiltinsCore.aSet((LispVector) elt, (LispInteger) idx, def);
                    return def;
                }
                else if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    int from = ((LispInteger)((LispList) idx).car()).getData();
                    int to = ((LispInteger)((LispList) idx).cdr()).getData();
                    if (to >= ((LispVector) elt).length())
                        to = ((LispVector) elt).length() - 1;
                    for (; from <= to; from++)
                        BuiltinsCore.aSet((LispVector) elt, new LispInteger(from), def);
                    if (to == ((LispInteger)((LispList) idx).cdr()).getData())
                        return def;
                }
                insertionPoint = (LispList) tail;
            }
            else if (elt instanceof LispCharTable) {
                if (BuiltinPredicates.isWholeNumber(idx) && (((LispInteger)idx).getData() & CharUtil.CHAR_MODIFIER_MASK) == 0) {
                    BuiltinsCore.aSet((LispArray)elt, (LispInteger) idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                    return def;
                }
                else if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    BuiltinsCharTable.setCharTableRange((LispCharTable) elt, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                    return def;
                }
                insertionPoint = (LispList) tail;
            }
            else if (elt instanceof LispList) {
                if (BuiltinsCore.eqs(idx, ((LispList) elt).car())) {
                    ((LispList) elt).setCdr(def);
                    return def;
                }
                else if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    int from = ((LispInteger)((LispList) idx).car()).getData();
                    int to = ((LispInteger)((LispList) idx).cdr()).getData();

                    if (from <= ((LispInteger)((LispList) idx).car()).getData()
                            && to >= ((LispInteger)((LispList) idx).car()).getData())  {
                        ((LispList) elt).setCdr(def);
                        if (from == to)
                            return def;
                    }
                }
            }
            else if (BuiltinsCore.eqs(elt, ourKeyMapSymbol)) {
                LispObject tmp;
                if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    tmp = BuiltinsCharTable.makeCharTable(ourKeyMapSymbol, LispSymbol.ourNil);
                    BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                }
                else
                    tmp = LispList.cons(idx, def);

                insertionPoint.setCdr(LispList.cons(tmp, insertionPoint.cdr()));
                return def;
            }
        }
        LispObject tmp;
        if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
            tmp = BuiltinsCharTable.makeCharTable(ourKeyMapSymbol, LispSymbol.ourNil);
            BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
        } else
            tmp = LispList.cons(idx, def);
        insertionPoint.setCdr(LispList.cons(tmp, insertionPoint.cdr()));
        return def;
    }

    private static void silly_event_symbol_error (LispObject c) {
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

    private static LispObject getKeyElement (LispObject object) {
        while (true) {
            if (!(object instanceof LispList))
                /* This is really the value.  */
                return object;
                /* If the keymap contents looks like (keymap ...) or (lambda ...) then use itself. */
            else if (BuiltinsCore.eqs (((LispList) object).car(), ourKeyMapSymbol) ||
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
        Keymap alist elements like (CHAR MENUSTRING . DEFN)
        will be used by HierarKey menus.  */
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
                LispKeymapImpl map1 = (LispKeymapImpl) getKeymap(((LispList) object).car());
                return (map1 == null ? object /* Invalid keymap */
                        : map1.accessKeymap(((LispList) object).cdr(), false, false));
            }
        }
    }

    private void fixSubmapInheritance(LispObject event, LispObject submap) {
        /* SUBMAP is a cons that we found as a key binding. Discard the other things found in a menu key binding.  */
        submap = getKeymap(getKeyElement(submap));
        /* If it isn't a keymap now, there's no work to do.  */
        if (submap == null)
            return;
        LispObject mapParent = keymapParent(this);
        LispObject parentEntry = !mapParent.equals(LispSymbol.ourNil)
                ? getKeymap(((LispKeymapImpl) mapParent).accessKeymap(event, false, false))
                : null;

        /* If MAP's parent has something other than a keymap, our own submap shadows it completely.  */
        if (parentEntry == null)
            return;

        if (! BuiltinsCore.eqs (parentEntry, submap)) {
            LispObject submapParent = submap;
            while (true) {
                LispObject tmp = keymapParent(submapParent);
                if (isKeymap(tmp)) {
                    if (((LispKeymapImpl)getKeymap(tmp)).keymapMemberP(parentEntry)) // Fset_keymap_parent could create a cycle.
                        return;
                    submapParent = tmp;
                }
                else
                    break;
            }
            setKeymapParent(submapParent, parentEntry);
        }
    }

    protected boolean isParentOf (LispKeymap keymap) {
        for (LispKeymap parent = keymap; parent != null; parent = parent.getParent()) {
            if (this == parent)
                return true;
        }
        return false;
    }

}
