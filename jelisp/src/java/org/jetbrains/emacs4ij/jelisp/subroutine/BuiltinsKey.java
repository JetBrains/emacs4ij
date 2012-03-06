package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeyBoardUtil;
import org.jetbrains.emacs4ij.jelisp.Pair;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
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

    static private LObject global_map;
    static private LObject current_global_map;
    /* Hash table used to cache a reverse-map to speed up calls to where-is.  */
    static LObject where_is_cache;
    /* Which keymaps are reverse-stored in the cache.  */
    static LObject where_is_cache_keymaps;

    private static boolean isListKeymap (LObject object) {
        return (object instanceof LispList && ((LispList) object).car().equals(new LispSymbol("keymap")));
    }

    private static boolean isKeymap (LObject object) {
        return (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction()))
                || isListKeymap(object);
    }

    private static LispList getKeymap(LObject object) {
        if (isListKeymap(object)) {
            return (LispList) object;
        }
        if (object instanceof LispSymbol && isListKeymap(((LispSymbol) object).getFunction())) {
            return (LispList) ((LispSymbol) object).getFunction();
        }
        throw new WrongTypeArgumentException("keymapp", object.toString());
    }

    private static void check (LObject object) {
        if (!isKeymap(object))
            throw new WrongTypeArgumentException("keymapp", object.toString());
    }

    @Subroutine("keymapp")
    public static LispSymbol keymapP (LObject object) {
        return LispSymbol.bool(isKeymap(object));
    }

    @Subroutine("make-sparse-keymap")
    public static LispList makeSparseKeymap (@Optional LObject prompt) {
        if (prompt != null && !prompt.equals(LispSymbol.ourNil)) {
            //todo prompt is String
            throw new RuntimeException("not implemented");
        }
        return LispList.list(new LispSymbol("keymap"));
    }

    //todo
    @Subroutine("make-keymap")
    public static LispList makeKeymap (@Optional LObject prompt) {
        if (prompt != null && !prompt.equals(LispSymbol.ourNil)) {
            throw new RuntimeException("not implemented");
        }
        return null;
    }

    //todo
    @Subroutine("copy-keymap")
    public static LispList copyKeymap (LObject object) {
        check(object);
        return null;
    }

    @Subroutine("keymap-parent")
    public static LObject keymapParent (LObject object) {
        LispList element = getKeymap(object);
        return element.tail();
    }

    @Subroutine("set-keymap-parent")
    public static LObject setKeymapParent (LObject object, LObject parent) {
        LispList element = getKeymap(object);
        check(parent);
        //todo: If keymap has submaps (bindings for prefix keys), they too receive new parent keymaps
        // that reflect what parent specifies for those prefix keys.
        element.resetTail(parent);
        return parent;
    }

    @Subroutine("define-prefix-command")
    public static LispSymbol definePrefixCommand (LispSymbol command, @Optional LObject mapVar, @Optional LObject name) {
        //todo: =)
        return command;
    }

    @Subroutine("current-active-maps")
    public static LObject currentActiveMaps (@Optional LObject olp, LObject position) {
        return null;
    }

    @Subroutine("key-binding")
    public static LObject keyBinding (LObject key, @Optional LObject acceptDefault, LObject noReMap, LObject position) {
        if (key instanceof LispString) {
            return null;
        }
        if (key instanceof LispVector) {
            return null;
        }
        throw new WrongTypeArgumentException("arrayp", key.toString());
    }

    @Subroutine("define-key")
    public static LObject defineKey(LispList keymap, LObject key, LObject function) {
        return LispSymbol.ourNil;
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

        /* Now we are ready to set up this property, so we can
    create char tables.  */
        LispSymbol k = new LispSymbol("keymap");
        k.setProperty("char-table-extra-slots", new LispInteger(0));
        g.defineSymbol(k);


        /* Initialize the keymaps standardly used.
  Each one is the value of a Lisp variable, and is also
  pointed to by a C variable */

        global_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("global-map"), global_map);
        current_global_map = global_map;
//        staticpro (&global_map);
//        staticpro (&current_global_map);
        LObject meta_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("esc-map"), meta_map);
        BuiltinsCore.functionSet(g, new LispSymbol("ESC-prefix"), meta_map);

        LObject control_x_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("ctl-x-map"), control_x_map);
        BuiltinsCore.functionSet(g, new LispSymbol("Control-X-prefix"), control_x_map);

        //here were used 'pure_cons' and 'make_pure_c_string'
        LispList exclude_keys = LispList.cons (LispList.cons (new LispString("DEL"), new LispString("\\d")),
                LispList.cons(LispList.cons(new LispString("TAB"), new LispString("\\t")),
                        LispList.cons(LispList.cons(new LispString("RET"), new LispString("\\r")),
                                LispList.cons(LispList.cons(new LispString("ESC"), new LispString("\\e")),
                                        LispList.cons(LispList.cons(new LispString("SPC"), new LispString(" ")),
                                                LispSymbol.ourNil)))));
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
//
        where_is_cache_keymaps = LispSymbol.ourT;
        where_is_cache = LispSymbol.ourNil;
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

    private static void keys_of_keymap () {
        initial_define_key (global_map, 27, "ESC-prefix");
        initial_define_key (global_map, CharUtil.Ctl('X'), "Control-X-prefix");
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name, LispSymbol parent) {
        LispSymbol keyMap = makeKeyMap(g, name);
        BuiltinsKey.setKeymapParent(keyMap, parent);
        return keyMap;
    }

    private static LispSymbol makeKeyMap(GlobalEnvironment g, String name) {
        return g.defineSymbol(name, BuiltinsKey.makeSparseKeymap(LispSymbol.ourNil));
    }

    private static void initial_define_key (LObject keymap, int key, String name) {
        store_in_keymap (keymap, new LispInteger(key), new LispString(name));
    }

    private static LObject store_in_keymap (LObject keymap, LObject idx, LObject def) {
        where_is_cache = LispSymbol.ourNil;
        where_is_cache_keymaps = LispSymbol.ourT;
        if (!isListKeymap(keymap))
            BuiltinsCore.error(GlobalEnvironment.INSTANCE, "attempt to define a key in a non-keymap");
        if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
            if (!BuiltinPredicates.isCharacter(((LispList) idx).cdr()))
                throw new WrongTypeArgumentException("characterp", ((LispList) idx).cdr().toString());
        } else
            idx = BuiltinPredicates.eventHead(idx);

        if (idx instanceof LispSymbol)
            idx = KeyBoardUtil.reorderModifiers(idx);
        else if (idx instanceof LispInteger)
            ((LispInteger) idx).setData(((LispInteger) idx).getData() & (CharUtil.CHAR_META | (CharUtil.CHAR_META - 1)));


        /* Scan the keymap for a binding of idx.  */

        LispList insertion_point = (LispList) keymap;
        for (LObject tail = ((LispList)keymap).cdr(); tail instanceof LispList; tail = ((LispList) tail).cdr())
        {
            LObject elt = ((LispList) tail).car();
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
                    BuiltinsCore.aSet((LispVector) elt, (LispInteger) idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
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
            else if (BuiltinsCore.eqs(elt, new LispSymbol("keymap"))) {
                LObject tmp;
                if (idx instanceof LispList && BuiltinPredicates.isCharacter(((LispList) idx).car())) {
                    tmp = BuiltinsCharTable.makeCharTable(new LispSymbol("keymap"), LispSymbol.ourNil);
                    BuiltinsCharTable.setCharTableRange((LispCharTable) tmp, idx, def.equals(LispSymbol.ourNil) ? LispSymbol.ourT : def);
                }
                else
                    tmp = LispList.cons(idx, def);
//                CHECK_IMPURE (insertion_point);

                insertion_point.setCdr(LispList.cons(tmp, insertion_point.cdr()));
            }
//                QUIT;
        }
        return def;
    }



    
    public static int parseModifiersUncached(LObject symbol, LispInteger lispModifierEnd) {
        if (!(symbol instanceof LispSymbol))
            throw new WrongTypeArgumentException("symbolp", symbol.toString());
        LispString name = new LispString(((LispSymbol) symbol).getName());

        int modifiers = 0;
        int i;

        for (i = 0; i + 2 <= name.lengthInBytes(); ) {
            int thisModEnd = 0;
            int thisMod = 0;
            Pair pair = CharUtil.Modifiers.check(name.getData().substring(i));
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

        if (0 == (modifiers & (CharUtil.Modifiers.DOWN.value() | CharUtil.Modifiers.DRAG.value()
                | CharUtil.Modifiers.DOUBLE.value() | CharUtil.Modifiers.TRIPLE.value()))
                && i + 7 == name.lengthInBytes()
                && name.getData().substring(i).startsWith("mouse-")
                && ('0' <= name.charAt(i + 6) && name.charAt(i + 6) <= '9'))
            modifiers |= CharUtil.Modifiers.CLICK.value();

        if (0 == (modifiers & (CharUtil.Modifiers.DOUBLE.value() | CharUtil.Modifiers.TRIPLE.value()))
                && i + 6 < name.lengthInBytes()
                && name.getData().substring(i).startsWith("wheel-"))
            modifiers |= CharUtil.Modifiers.CLICK.value();

        lispModifierEnd.setData(i);
        return modifiers;
    }


}
