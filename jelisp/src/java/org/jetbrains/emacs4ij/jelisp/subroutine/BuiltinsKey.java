package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.*;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/15/12
 * Time: 3:31 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsKey {
    private BuiltinsKey() {}

    public static LispObject globalMap;
    private static LispObject currentGlobalMap;
    private static LispList exclude_keys;

    private static LispSymbol myKeyMapSymbol = new LispSymbol("keymap");

    private static boolean isListKeymap (LispObject object) {
        return (object instanceof LispList && ((LispList) object).car().equals(myKeyMapSymbol));
    }

    private static boolean isKeymap (LispObject object) {
        return (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction()))
                || isListKeymap(object);
    }

    private static LispList getKeymap(LispObject object) {
        if (isListKeymap(object)) {
            return (LispList) object;
        }
        if (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction())) {
            return (LispList) ((LispSymbol) object).getFunction();
        }
        return null;
    }

    private static void check (LispObject object) {
        if (!isKeymap(object))
            throw new WrongTypeArgumentException("keymapp", object);
    }

    @Subroutine("keymapp")
    public static LispSymbol keymapP (LispObject object) {
        return LispSymbol.bool(isKeymap(object));
    }

    @Subroutine("make-sparse-keymap")
    public static LispList makeSparseKeymap (@Nullable @Optional LispObject prompt) {
        if (prompt != null && !prompt.equals(LispSymbol.ourNil)) {
            //todo prompt is String
            throw new NotImplementedException();
        }
        return LispList.list(myKeyMapSymbol);
    }

    @Subroutine("make-keymap")
    public static LispList makeKeymap (@Optional LispObject prompt) {
        //note: though prompt is supposed to be a string, any object is accepted in Emacs
        LispObject tail = LispSymbol.ourNil;
        if (prompt != null && !prompt.equals(LispSymbol.ourNil)) {
            tail = LispList.list(prompt);
        }
        return LispList.cons(myKeyMapSymbol,
                LispList.cons(new LispCharTable(myKeyMapSymbol, LispSymbol.ourNil), tail));
    }

    @Subroutine("copy-keymap")
    public static LispList copyKeymap (LispObject object) {
        check(object);
        throw new NotImplementedException();
    }

    @Subroutine("keymap-parent")
    public static LispObject keymapParent (LispObject object) {
        LispList element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        return element.tail();
    }

    @Subroutine("set-keymap-parent")
    public static LispObject setKeymapParent (LispObject object, LispObject parent) {
        LispList element = getKeymap(object);
        if (element == null)
            throw new WrongTypeArgumentException("keymapp", object);
        check(parent);
        //todo: If keymap has submaps (bindings for prefix keys), they too receive new parent keymaps
        // that reflect what parent specifies for those prefix keys.
        element.resetTail(parent);
        return parent;
    }

    @Subroutine("define-prefix-command")
    public static LispSymbol definePrefixCommand (LispSymbol command, @Optional LispObject mapVar, @Optional LispObject name) {
        throw new NotImplementedException();
//        return command;
    }

    @Subroutine("current-active-maps")
    public static LispObject currentActiveMaps (@Optional LispObject olp, LispObject position) {
        return null;
    }

    @Subroutine("key-binding")
    public static LispObject keyBinding (LispObject key, @Optional LispObject acceptDefault, LispObject noReMap, LispObject position) {
        if (key instanceof LispString) {
            return null;
        }
        if (key instanceof LispVector) {
            return null;
        }
        throw new WrongTypeArgumentException("arrayp", key);
    }

    @Subroutine("define-key")
    public static LispObject defineKey(Environment environment, LispList keymap, LispStringOrVector key, LispObject function) {
        check(keymap);
//        if (!(key instanceof LispString) && !(key instanceof LispVector))
//            throw new WrongTypeArgumentException("arrayp", key);
        int length = ((LispSequence)key).length();
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
                if (c instanceof LispList && lucid_event_type_list_p (c))
                    c = eventConvertList(c);
                tmp.setItem(i, c);
            }
            //todo?
            function = tmp;
        }

        int idx = 0;
        int metized = 0;
        while (true) {
            LispObject c = BuiltinsCore.aRef((LispArray) key, new LispInteger(idx));
            if (c instanceof LispList) {
                if (lucid_event_type_list_p (c))
                    c = eventConvertList(c);
                else if (BuiltinPredicates.isCharacter(((LispList) c).car())) {
                    if (!BuiltinPredicates.isCharacter(((LispList) c).cdr()))
                        throw new WrongTypeArgumentException("characterp", ((LispList) c).cdr());
                }
            }

            if (c instanceof LispSymbol)
                silly_event_symbol_error (c);

            if (c instanceof LispInteger && ((((LispInteger) c).getData() & meta_bit) != 0) && metized == 0) {
                c = GlobalEnvironment.INSTANCE.find("meta_prefix_char").getValue();
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
                BuiltinsCore.error(GlobalEnvironment.INSTANCE, "Key sequence contains invalid event");

            if (idx == length)
                return store_in_keymap (keymap, c, function);

            LispObject cmd = accessKeyMap(keymap, c, 0, 1);

            /* If this key is undefined, make it a prefix.  */
            if (cmd.equals(LispSymbol.ourNil))
                cmd = define_as_prefix (keymap, c);

            keymap = getKeymap(cmd);
            if (keymap == null)
                BuiltinsCore.error(environment, String.format("Key sequence %s starts with non-prefix key %s",
                        keyDescription(environment, key, LispSymbol.ourNil).getData(),
                        keyDescription(environment, BuiltinsCore.substring(key, new LispInteger(0),
                                new LispInteger(idx)),
                                LispSymbol.ourNil).getData()));
        }
    }

    private static LispObject define_as_prefix (LispObject keymap, LispObject c) {
        LispObject cmd = makeSparseKeymap(null);
        cmd = BuiltinsList.nConcatenate(cmd, accessKeyMap (keymap, c, 0, 0));
        store_in_keymap (keymap, c, cmd);
        return cmd;
    }

    @Subroutine("key-description")
    public static LispString keyDescription (Environment environment, LispObject keys, @Optional LispObject prefix) {
        String first = (prefix == null || prefix.equals(LispSymbol.ourNil))
                ? ""
                : BuiltinsSequence.mapConcat(environment, new LispSymbol("single-key-description"), prefix, new LispString(" ")).getData();
        String second = BuiltinsSequence.mapConcat(environment, new LispSymbol("single-key-description"), keys, new LispString(" ")).getData();
        return new LispString(first + " " + second);
    }

    @Subroutine("current-global-map")
    public static LispObject currentGlobalMap () {
        return currentGlobalMap;
    }

    public static void defineKeyMaps (GlobalEnvironment g) {
        //staticpro -- is a protection from elisp GC
//        staticpro (&Qkeymap);
//        staticpro (&apropos_predicate);
//        staticpro (&apropos_accumulate);
//        apropos_predicate = LispSymbol.ourNil;
//        apropos_accumulate = LispSymbol.ourNil;
//
//        Qkeymap_canonicalize = new LispString("keymap-canonicalize");
//        staticpro (&Qkeymap_canonicalize);

        myKeyMapSymbol.setProperty("char-table-extra-slots", new LispInteger(0));
        g.defineSymbol(myKeyMapSymbol);

        globalMap = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("global-map"), globalMap);
        currentGlobalMap = globalMap;
//        staticpro (&global_map);
//        staticpro (&current_global_map);
        LispObject meta_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("esc-map"), meta_map);
        BuiltinsCore.functionSet(g, new LispSymbol("ESC-prefix"), meta_map);

        LispObject control_x_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("ctl-x-map"), control_x_map);
        BuiltinsCore.functionSet(g, new LispSymbol("Control-X-prefix"), control_x_map);

        //here were used 'pure_cons' and 'make_pure_c_string'
        exclude_keys = LispList.list (LispList.cons(new LispString("DEL"), new LispString("\\d")),
                LispList.cons(new LispString("TAB"), new LispString("\\t")),
                LispList.cons(new LispString("RET"), new LispString("\\r")),
                LispList.cons(new LispString("ESC"), new LispString("\\e")),
                LispList.cons(new LispString("SPC"), new LispString(" ")));
//        staticpro (&exclude_keys);


        g.defineSymbol("define-key-rebound-commands", LispSymbol.ourT);
        LispSymbol mblMap   = makeKeyMap(g, "minibuffer-local-map");
        LispSymbol mblNsMap = makeKeyMap(g, "minibuffer-local-ns-map", mblMap);
        LispSymbol mblCompletionMap = makeKeyMap(g, "minibuffer-local-completion-map", mblMap);
        LispSymbol mblFileNameCompletionMap = makeKeyMap(g, "minibuffer-local-filename-completion-map", mblCompletionMap);
        LispSymbol mblMustMatchMap = makeKeyMap(g, "minibuffer-local-must-match-map", mblCompletionMap);
        LispSymbol mblFileNameMustMatchMap = makeKeyMap(g, "minibuffer-local-filename-must-match-map", mblMustMatchMap);

        g.defineSymbol("minor-mode-map-alist");
        g.defineSymbol("minor-mode-overriding-map-alist");
        g.defineSymbol("emulation-mode-map-alists");
        g.defineSymbol("where-is-preferred-modifier");
//        where_is_preferred_modifier = 0;

//        staticpro (&Vmouse_events);

        LispList mouse_events = LispList.list(new LispString("menu-bar"), new LispString("menu-bar"), new LispString("tool-bar"),
                new LispString("header-line"), new LispString("mode-line"),new LispString("mouse-1"), new LispString("mouse-2"),
                new LispString("mouse-3"), new LispString("mouse-4"), new LispString("mouse-5"));


//        Qsingle_key_description = new LispString("single-key-description");
//        staticpro (&Qsingle_key_description);

//        Qkey_description = new LispString("key-description");
//        staticpro (&Qkey_description);

//        Qkeymapp = new LispString("keymapp");
//        staticpro (&Qkeymapp);
//
//        Qnon_ascii = new LispString("non-ascii");
//        staticpro (&Qnon_ascii);
//
//        Qmenu_item = new LispString("menu-item");
//        staticpro (&Qmenu_item);
//
//        Qremap = new LispString("remap");
//        staticpro (&Qremap);
//
//        QCadvertised_binding = new LispString(":advertised-binding");
//        staticpro (&QCadvertised_binding);

//        command_remapping_vector = Fmake_vector (make_number (2), Qremap);
//        staticpro (&command_remapping_vector);
////
//        where_is_cache_keymaps = LispSymbol.ourT;
//        where_is_cache = LispSymbol.ourNil;


//        staticpro (&where_is_cache);
//        staticpro (&where_is_cache_keymaps);
//
//        defsubr (&Skeymapp);
//        defsubr (&Skeymap_parent);
//        defsubr (&Skeymap_prompt);
//        defsubr (&Sset_keymap_parent);
//        defsubr (&Smake_keymap);
//        defsubr (&Smake_sparse_keymap);
//        defsubr (&Smap_keymap_internal);
//        defsubr (&Smap_keymap);
//        defsubr (&Scopy_keymap);
//        defsubr (&Scommand_remapping);
//        defsubr (&Skey_binding);
//        defsubr (&Slocal_key_binding);
//        defsubr (&Sglobal_key_binding);
//        defsubr (&Sminor_mode_key_binding);
//        defsubr (&Sdefine_key);
//        defsubr (&Slookup_key);
//        defsubr (&Sdefine_prefix_command);
//        defsubr (&Suse_global_map);
//        defsubr (&Suse_local_map);
//        defsubr (&Scurrent_local_map);
//        defsubr (&Scurrent_global_map);
//        defsubr (&Scurrent_minor_mode_maps);
//        defsubr (&Scurrent_active_maps);
//        defsubr (&Saccessible_keymaps);
//        defsubr (&Skey_description);
//        defsubr (&Sdescribe_vector);
//        defsubr (&Ssingle_key_description);
//        defsubr (&Stext_char_description);
//        defsubr (&Swhere_is_internal);
//        defsubr (&Sdescribe_buffer_bindings);
//        defsubr (&Sapropos_internal);
    }

    public static void keys_of_keymap () {
        initial_define_key (globalMap, 27, "ESC-prefix");
        initial_define_key (globalMap, CharUtil.Ctl('X'), "Control-X-prefix");
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name, LispSymbol parent) {
        LispSymbol keyMap = makeKeyMap(g, name);
        BuiltinsKey.setKeymapParent(keyMap.getValue(), parent.getValue());
        return keyMap;
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name) {
        return g.defineSymbol(name, BuiltinsKey.makeSparseKeymap(LispSymbol.ourNil));
    }

    public static void initial_define_key (LispObject keymap, int key, String name) {
        store_in_keymap (keymap, new LispInteger(key), new LispString(name));
    }

    private static LispObject store_in_keymap (LispObject keymap, LispObject idx, LispObject def) {
        if (!isListKeymap(keymap))
            BuiltinsCore.error(GlobalEnvironment.INSTANCE, "attempt to define a key in a non-keymap");
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

        LispList insertion_point = (LispList) keymap;
        for (LispObject tail = ((LispList)keymap).cdr(); tail instanceof LispList; tail = ((LispList) tail).cdr())
        {
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
                insertion_point = (LispList) tail;
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
                insertion_point = (LispList) tail;
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
            else if (BuiltinsCore.eqs(elt, myKeyMapSymbol)) {
                LispObject tmp;
                if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    tmp = BuiltinsCharTable.makeCharTable(myKeyMapSymbol, LispSymbol.ourNil);
                    BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                }
                else
                    tmp = LispList.cons(idx, def);
//                CHECK_IMPURE (insertion_point);

                insertion_point.setCdr(LispList.cons(tmp, insertion_point.cdr()));
                return def;
            }
//                QUIT;
        }
        LispObject tmp;
        if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
            tmp = BuiltinsCharTable.makeCharTable(myKeyMapSymbol, LispSymbol.ourNil);
            BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
        }
        else
            tmp = LispList.cons(idx, def);
//                CHECK_IMPURE (insertion_point);

        insertion_point.setCdr(LispList.cons(tmp, insertion_point.cdr()));
        return def;
    }

    public static int parseModifiersUncached(LispObject symbol, LispInteger lispModifierEnd) {
        if (!(symbol instanceof LispSymbol))
            throw new WrongTypeArgumentException("symbolp", symbol);
        LispString name = new LispString(((LispSymbol) symbol).getName());

        int modifiers = 0;
        int i;

        for (i = 0; i + 2 <= name.lengthInBytes(); ) {
            int thisModEnd = 0;
            int thisMod = 0;
            Pair pair = KeyBoardModifier.check(name.getData().substring(i));
            if (pair != null) {
                thisModEnd = i + pair.getLength();
                thisMod = pair.getModifier();
            }
            if (thisModEnd == 0)
                break;
            if (thisModEnd >= name.lengthInBytes() || name.charAt(thisModEnd) != '-')
                break;

            modifiers |= thisMod;
            i = thisModEnd + 1;
        }

        if (0 == (modifiers & KeyBoardModifier.bitwiseOr(KeyBoardModifier.DOWN, KeyBoardModifier.DRAG,
                              KeyBoardModifier.DOUBLE, KeyBoardModifier.TRIPLE))
                && i + 7 == name.lengthInBytes()
                && name.getData().substring(i).startsWith("mouse-")
                && ('0' <= name.charAt(i + 6) && name.charAt(i + 6) <= '9'))
            modifiers = KeyBoardModifier.CLICK.bitwiseOr(modifiers);

        if (0 == (modifiers & KeyBoardModifier.bitwiseOr(KeyBoardModifier.DOUBLE, KeyBoardModifier.TRIPLE))
                && i + 6 < name.lengthInBytes()
                && name.getData().substring(i).startsWith("wheel-"))
            modifiers = KeyBoardModifier.CLICK.bitwiseOr(modifiers);

        lispModifierEnd.setData(i);
        return modifiers;
    }

    private static boolean lucid_event_type_list_p (LispObject object) {
        for (LispObject tail = object; tail instanceof LispList; tail = ((LispList) tail).cdr()) {
            LispObject item = ((LispList) tail).car();
            if (!(item instanceof LispInteger || item instanceof LispSymbol))
                return false;
        }
        return true;
    }

    @Subroutine("event-convert-list")
    public static LispObject eventConvertList (LispObject event_desc) {
        LispObject base = LispSymbol.ourNil;
        LispObject rest = event_desc;
        int modifiers = 0;
        while (rest instanceof LispList) {
            LispObject item = ((LispList) rest).car();
            rest = ((LispList) rest).cdr();
            int index = 0;
            /* Given a symbol, see if it is a modifier name.  */
            if (item instanceof LispSymbol && rest instanceof LispList)
                index = KeyBoardModifier.parseSolitary((LispSymbol) item);
            if (index != 0)
                modifiers |= index;
            else if (!base.equals(LispSymbol.ourNil))
                BuiltinsCore.error(GlobalEnvironment.INSTANCE, "Two bases given in one event");
            else
                base = item;
        }
        /* Let the symbol A refer to the character A.  */
        if (base instanceof LispSymbol && ((LispSymbol) base).getName().length() == 1)
            base = new LispInteger(((LispSymbol) base).getName().charAt(0));

        if (base instanceof LispInteger) {
            /* Turn (shift a) into A.  */
            if (KeyBoardModifier.SHIFT.bitwiseAndNotZero(modifiers)
                    && (((LispInteger) base).getData() >= 'a' && ((LispInteger) base).getData() <= 'z'))
            {
                ((LispInteger) base).setData(((LispInteger) base).getData() - ('a' - 'A'));
                modifiers = KeyBoardModifier.SHIFT.bitwiseAndNot(modifiers);
            }

            /* Turn (control a) into C-a.  */
            if (KeyBoardModifier.CTRL.bitwiseAndNotZero(modifiers))
                return new LispInteger(KeyBoardModifier.CTRL.bitwiseAndNot(modifiers)
                        | CharUtil.makeCtrlChar(((LispInteger) base).getData()));
            else
                return new LispInteger(modifiers | ((LispInteger) base).getData());
        }
        else if (base instanceof LispSymbol)
            return KeyBoardUtil.applyModifiers(modifiers, base);
        else {
            BuiltinsCore.error(GlobalEnvironment.INSTANCE, "Invalid base event");
            return LispSymbol.ourNil;
        }
    }

    private static void silly_event_symbol_error (LispObject c) {
        LispList parsed = KeyBoardUtil.parseModifiers(c);
        int modifiers = ((LispInteger)((LispList)parsed.cdr()).car()).getData();
        LispSymbol base = (LispSymbol) parsed.car();
        LispList assoc = BuiltinsList.assoc(new LispString(base.getName()), exclude_keys);
        if (!assoc.isEmpty()) {
            String new_mods = KeyBoardModifier.test(modifiers);
            c = KeyBoardUtil.reorderModifiers(c);
            String keyString = new_mods + assoc.cdr().toString();
            if (!(c instanceof LispSymbol))
                throw new WrongTypeArgumentException("silly_event_symbol_error: symbolp ", c);
            BuiltinsCore.error (GlobalEnvironment.INSTANCE,
                    String.format((KeyBoardModifier.META.bitwiseAndNotNotZero(modifiers)
                            ? "To bind the key %s, use [?%s], not [%s]"
                            : "To bind the key %s, use \"%s\", not [%s]"),
                    ((LispSymbol) c).getName(), keyString,
                    ((LispSymbol) c).getName()));
        }
    }

    private static LispObject accessKeyMap (LispObject map, LispObject idx, int t_ok, int noinherit) {
        LispSymbol unbound = new LispSymbol("unbound");
        LispObject val = unbound;

        idx = BuiltinPredicates.eventHead(idx);

        /* If idx is a symbol, it might have modifiers, which need to be put in the canonical order.  */
        if (idx instanceof LispSymbol)
            idx = KeyBoardUtil.reorderModifiers(idx);
        else if (idx instanceof LispInteger)
            ((LispInteger) idx).setData(((LispInteger) idx).getData() & (CharUtil.CHAR_META | (CharUtil.CHAR_META - 1)));

        /* Handle the special meta -> esc mapping. */
        if (idx instanceof LispInteger && KeyBoardModifier.META.bitwiseAndNotZero((LispInteger) idx)) {
            /* See if there is a meta-map.  If there's none, there is
no binding for IDX, unless a default binding exists in MAP.  */
            /* A strange value in which Meta is set would cause
        infinite recursion.  Protect against that.  */
            LispInteger meta_prefix_char = (LispInteger)GlobalEnvironment.INSTANCE.find("meta-prefix-char").getValue();

            if ((meta_prefix_char.getData() & CharUtil.CHAR_META) != 0)
                meta_prefix_char.setData(27);
            LispObject meta_map = accessKeyMap(map, meta_prefix_char, t_ok, noinherit);
            if (meta_map instanceof LispList) {
                map = meta_map;
                ((LispInteger) idx).setData(KeyBoardModifier.META.bitwiseAndNot((LispInteger) idx));
            }
            else if (t_ok != 0)
                /* Set IDX to t, so that we only find a default binding.  */
                idx = LispSymbol.ourT;
            else
                /* We know there is no binding.  */
                return LispSymbol.ourNil;
        }

        /* t_binding is where we put a default binding that applies,
   to use in case we do not find a binding specifically
   for this key sequence.  */

        LispObject t_binding = LispSymbol.ourNil;
        if (map instanceof LispList)
            for (LispObject tail = ((LispList) map).cdr();
                 tail instanceof LispList || ((tail = getKeymap (tail)) != null);
                 tail = ((LispList)tail).cdr())
            {
                LispObject binding = ((LispList)tail).car();
                if (binding instanceof LispSymbol) {
                    /* If NOINHERIT, stop finding prefix definitions after we pass a second occurrence of the `keymap' symbol.  */
                    if (noinherit != 0 && BuiltinsCore.eqs(binding, myKeyMapSymbol))
                        return LispSymbol.ourNil;
                }
                else if (binding instanceof LispList) {
                    LispObject key = ((LispList) binding).car();
                    if (BuiltinsCore.eqs(key, idx))
                        val = ((LispList) binding).cdr();
                    else if (t_ok != 0 && BuiltinsCore.eqs(key, LispSymbol.ourT)) {
                        t_binding = ((LispList) binding).cdr();
                        t_ok = 0;
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
                        fix_submap_inheritance (map, idx, val);
                    return val;
                }
//            QUIT;
            }
        return getKeyElement(t_binding);
    }

    private static void fix_submap_inheritance (LispObject map, LispObject event, LispObject submap) {
        /* SUBMAP is a cons that we found as a key binding. Discard the other things found in a menu key binding.  */
        submap = getKeymap(getKeyElement(submap));
        /* If it isn't a keymap now, there's no work to do.  */
        if (submap == null)
            return;

        LispObject map_parent = keymapParent(map);
        LispObject parent_entry = !map_parent.equals(LispSymbol.ourNil)
                ? getKeymap(accessKeyMap(map_parent, event, 0, 0))
                : null;

        /* If MAP's parent has something other than a keymap, our own submap shadows it completely.  */
        if (parent_entry == null)
            return;

        if (! BuiltinsCore.eqs (parent_entry, submap)) {
            LispObject submap_parent = submap;
            while (true) {
                LispObject tem = keymapParent(submap_parent);
                if (isKeymap(tem)) {
                    if (keymap_memberp (tem, parent_entry))
                        /* Fset_keymap_parent could create a cycle.  */
                        return;
                    submap_parent = tem;
                }
                else
                    break;
            }
            setKeymapParent(submap_parent, parent_entry);
        }
    }

    /* Check whether MAP is one of MAPS parents.  */
    private static boolean keymap_memberp (LispObject map, LispObject maps) {
        if (map.equals(LispSymbol.ourNil))
            return false;
        while (isKeymap(maps) && !BuiltinsCore.eqs(map, maps))
            maps = keymapParent(maps);
        return (BuiltinsCore.eqs(map, maps));
    }

    private static LispObject getKeyElement (LispObject object) {
        while (true) {
            if (!(object instanceof LispList))
                /* This is really the value.  */
                return object;
                /* If the keymap contents looks like (keymap ...) or (lambda ...) then use itself. */
            else if (BuiltinsCore.eqs (((LispList) object).car(), myKeyMapSymbol) ||
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
                LispObject map1 = getKeymap(((LispList) object).car());//, 0, autoload);
                return (map1 == null ? object /* Invalid keymap */
                        : accessKeyMap(map1, ((LispList) object).cdr(), 0, 0));
            }
        }
    }

    @Subroutine("single-key-description")
    public static LispObject singleKeyDescription (LispObject key, @Optional LispObject no_angles) {
        if (key instanceof LispList && lucid_event_type_list_p (key))
            key = eventConvertList(key);
        key = BuiltinPredicates.eventHead(key);
        if (key instanceof LispInteger) {		/* Normal character */
            String p = CharUtil.pushKeyDescription (((LispInteger) key).getData(), true);
            return new LispString(p); //make_specified_string (tem, -1, p - tem, 1);
        }
        else if (key instanceof LispSymbol)	{ /* Function key or event-symbol */
            if (no_angles == null || no_angles.equals(LispSymbol.ourNil))
                return new LispString('<' + ((LispSymbol) key).getName() + '>');
            else
                return new LispString(((LispSymbol) key).getName());
        }
        else if (key instanceof LispString)	/* Buffer names in the menubar.  */
            return BuiltinsSequence.copySequence(key);
        else
            BuiltinsCore.error (GlobalEnvironment.INSTANCE, "KEY must be an integer, cons, symbol, or string");
        return LispSymbol.ourNil;
    }




}
