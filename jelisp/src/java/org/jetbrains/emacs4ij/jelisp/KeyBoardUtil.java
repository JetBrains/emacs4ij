package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsList;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsSymbol;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/6/12
 * Time: 4:00 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class KeyBoardUtil {
    public static String ModifierNames[] = {"up", "down", "drag", "click", "double", "triple", null, null,
            null, null, null, null, null, null, null, null, null, null, null, null,
            null, null, "alt", "super", "hyper", "shift", "control", "meta"};

    public static int NUM_MOD_NAMES = ModifierNames.length;
    static LispVector modifier_symbols;

    /* Return the list of modifier symbols corresponding to the mask MODIFIERS.  */
    public static LispObject lispyModifierList(int modifiers) {
        LispObject modifierList = LispSymbol.ourNil;
        for (int i = 0; (1 << i) <= modifiers && i < NUM_MOD_NAMES; i++)
            if ((modifiers & (1 << i)) != 0)
                modifierList = LispList.cons(modifier_symbols.getItem(i), modifierList);
        return modifierList;
    }

    private static LispSymbol applyModifiersUncached (int modifiers, String base) {
        //todo: deal with multibyte chars
        String newMods = KeyBoardModifier.apply(modifiers);
        return BuiltinsSymbol.intern(new LispString(newMods + base), LispSymbol.ourNil);
    }

    public static LispObject applyModifiers (int modifiers, LispObject base) {
        modifiers &= LispNumber.INTMASK;
        if (base instanceof LispInteger)
            return new LispInteger(((LispInteger) base).getData() | modifiers);
        if (!(base instanceof LispSymbol))
            throw new WrongTypeArgumentException("symbolp", base);
        LispObject cache = ((LispSymbol) base).getProperty("modifier-cache");
        LispInteger index = new LispInteger(modifiers & ~KeyBoardModifier.CLICK.value);
        LispObject entry = BuiltinsCore.assqNoQuit(index, cache);
        LispSymbol newSymbol;
        if (entry instanceof LispList)
            newSymbol = (LispSymbol) ((LispList) entry).cdr();
        else {
            newSymbol = applyModifiersUncached(modifiers, ((LispSymbol) base).getName());
            entry = LispList.cons (index, newSymbol);
            ((LispSymbol) base).setProperty("modifier-cache", LispList.cons(entry, cache));
        }
        if (newSymbol.getProperty("event-kind").equals(LispSymbol.ourNil)) {
            LispObject kind = ((LispSymbol) base).getProperty("event-kind");
            if (!kind.equals(LispSymbol.ourNil))
                newSymbol.setProperty("event-kind", kind);
        }
        return newSymbol;
    }

    public static LispList parseModifiers (LispObject symbol) {
        if (symbol instanceof LispInteger)
            return LispList.list (new LispInteger(((LispInteger) symbol).keyToChar()),
                    new LispInteger (((LispInteger) symbol).getData() & CharUtil.CHAR_MODIFIER_MASK));
        else if (!(symbol instanceof LispSymbol))
            return LispList.list();
        return ((LispSymbol) symbol).parseModifiers();
    }

    public static LispObject reorderModifiers (LispObject symbol) {
        LispList parsed = parseModifiers (symbol);
        try {
            return applyModifiers (((LispInteger) BuiltinsList.car(parsed.cdr())).getData(), parsed.car());
        } catch (ClassCastException e) {
            throw new LispException("Internal error");
        }
    }

    public static void defineKbdSymbols (GlobalEnvironment g) {
//        pending_funcalls = Qnil;
//        staticpro (&pending_funcalls);
//
//        Vlispy_mouse_stem = make_pure_c_string ("mouse");
//        staticpro (&Vlispy_mouse_stem);
//
//        /* Tool-bars.  */
//        QCimage = intern_c_string (":image");
//        staticpro (&QCimage);
//
//        staticpro (&Qhelp_echo);
//        Qhelp_echo = intern_c_string ("help-echo");
//
//        staticpro (&Qrtl);
//        Qrtl = intern_c_string (":rtl");
//
//        staticpro (&item_properties);
//        item_properties = Qnil;
//
//        staticpro (&tool_bar_item_properties);
//        tool_bar_item_properties = Qnil;
//        staticpro (&tool_bar_items_vector);
//        tool_bar_items_vector = Qnil;
//
//        staticpro (&real_this_command);
//        real_this_command = Qnil;
//
//        Qtimer_event_handler = intern_c_string ("timer-event-handler");
//        staticpro (&Qtimer_event_handler);
//
//        Qdisabled_command_function = intern_c_string ("disabled-command-function");
//        staticpro (&Qdisabled_command_function);
//
//        Qself_insert_command = intern_c_string ("self-insert-command");
//        staticpro (&Qself_insert_command);
//
//        Qforward_char = intern_c_string ("forward-char");
//        staticpro (&Qforward_char);
//
//        Qbackward_char = intern_c_string ("backward-char");
//        staticpro (&Qbackward_char);
//
//        Qdisabled = intern_c_string ("disabled");
//        staticpro (&Qdisabled);
//
//        Qundefined = intern_c_string ("undefined");
//        staticpro (&Qundefined);
//
//        Qpre_command_hook = intern_c_string ("pre-command-hook");
//        staticpro (&Qpre_command_hook);
//
//        Qpost_command_hook = intern_c_string ("post-command-hook");
//        staticpro (&Qpost_command_hook);
//
//        Qdeferred_action_function = intern_c_string ("deferred-action-function");
//        staticpro (&Qdeferred_action_function);
//
//        Qcommand_hook_internal = intern_c_string ("command-hook-internal");
//        staticpro (&Qcommand_hook_internal);
//
//        Qfunction_key = intern_c_string ("function-key");
//        staticpro (&Qfunction_key);
//        Qmouse_click = intern_c_string ("mouse-click");
//        staticpro (&Qmouse_click);
//        #if defined (WINDOWSNT)
//        Qlanguage_change = intern_c_string ("language-change");
//        staticpro (&Qlanguage_change);
//        #endif
//            Qdrag_n_drop = intern_c_string ("drag-n-drop");
//        staticpro (&Qdrag_n_drop);
//
//        Qsave_session = intern_c_string ("save-session");
//        staticpro (&Qsave_session);
//
//        #ifdef HAVE_DBUS
//        Qdbus_event = intern_c_string ("dbus-event");
//        staticpro (&Qdbus_event);
//        #endif
//
//            Qconfig_changed_event = intern_c_string ("config-changed-event");
//        staticpro (&Qconfig_changed_event);
//
//        Qmenu_enable = intern_c_string ("menu-enable");
//        staticpro (&Qmenu_enable);
//        QCenable = intern_c_string (":enable");
//        staticpro (&QCenable);
//        QCvisible = intern_c_string (":visible");
//        staticpro (&QCvisible);
//        QChelp = intern_c_string (":help");
//        staticpro (&QChelp);
//        QCfilter = intern_c_string (":filter");
//        staticpro (&QCfilter);
//        QCbutton = intern_c_string (":button");
//        staticpro (&QCbutton);
//        QCkeys = intern_c_string (":keys");
//        staticpro (&QCkeys);
//        QCkey_sequence = intern_c_string (":key-sequence");
//        staticpro (&QCkey_sequence);
//        QCtoggle = intern_c_string (":toggle");
//        staticpro (&QCtoggle);
//        QCradio = intern_c_string (":radio");
//        staticpro (&QCradio);
//
//        Qmode_line = intern_c_string ("mode-line");
//        staticpro (&Qmode_line);
//        Qvertical_line = intern_c_string ("vertical-line");
//        staticpro (&Qvertical_line);
//        Qvertical_scroll_bar = intern_c_string ("vertical-scroll-bar");
//        staticpro (&Qvertical_scroll_bar);
//        Qmenu_bar = intern_c_string ("menu-bar");
//        staticpro (&Qmenu_bar);
//
//        #if defined (HAVE_MOUSE) || defined (HAVE_GPM)
//        Qmouse_fixup_help_message = intern_c_string ("mouse-fixup-help-message");
//        staticpro (&Qmouse_fixup_help_message);
//        #endif
//
//            Qabove_handle = intern_c_string ("above-handle");
//        staticpro (&Qabove_handle);
//        Qhandle = intern_c_string ("handle");
//        staticpro (&Qhandle);
//        Qbelow_handle = intern_c_string ("below-handle");
//        staticpro (&Qbelow_handle);
//        Qup = intern_c_string ("up");
//        staticpro (&Qup);
//        Qdown = intern_c_string ("down");
//        staticpro (&Qdown);
//        Qtop = intern_c_string ("top");
//        staticpro (&Qtop);
//        Qbottom = intern_c_string ("bottom");
//        staticpro (&Qbottom);
//        Qend_scroll = intern_c_string ("end-scroll");
//        staticpro (&Qend_scroll);
//        Qratio = intern_c_string ("ratio");
//        staticpro (&Qratio);
//
//        Qevent_kind = intern_c_string ("event-kind");
//        staticpro (&Qevent_kind);
//        Qevent_symbol_elements = intern_c_string ("event-symbol-elements");
//        staticpro (&Qevent_symbol_elements);
//        Qevent_symbol_element_mask = intern_c_string ("event-symbol-element-mask");
//        staticpro (&Qevent_symbol_element_mask);
//        Qmodifier_cache = intern_c_string ("modifier-cache");
//        staticpro (&Qmodifier_cache);
//
//        Qrecompute_lucid_menubar = intern_c_string ("recompute-lucid-menubar");
//        staticpro (&Qrecompute_lucid_menubar);
//        Qactivate_menubar_hook = intern_c_string ("activate-menubar-hook");
//        staticpro (&Qactivate_menubar_hook);
//
//        Qpolling_period = intern_c_string ("polling-period");
//        staticpro (&Qpolling_period);
//
//        Qinput_method_function = intern_c_string ("input-method-function");
//        staticpro (&Qinput_method_function);
//
//        Qinput_method_exit_on_first_char = intern_c_string ("input-method-exit-on-first-char");
//        staticpro (&Qinput_method_exit_on_first_char);
//        Qinput_method_use_echo_area = intern_c_string ("input-method-use-echo-area");
//        staticpro (&Qinput_method_use_echo_area);
//
//        Fset (Qinput_method_exit_on_first_char, Qnil);
//        Fset (Qinput_method_use_echo_area, Qnil);
//
//        last_point_position_buffer = Qnil;
//        last_point_position_window = Qnil;
//
//        {
//            struct event_head *p;
//
//            for (p = head_table;
//                 p < head_table + (sizeof (head_table) / sizeof (head_table[0]));
//                 p++)
//            {
//                *p->var = intern_c_string (p->name);
//                staticpro (p->var);
//                Fput (*p->var, Qevent_kind, *p->kind);
//                Fput (*p->var, Qevent_symbol_elements, Fcons (*p->var, Qnil));
//            }
//        }
//
//        button_down_location = Fmake_vector (make_number (5), Qnil);
//        staticpro (&button_down_location);
//        mouse_syms = Fmake_vector (make_number (5), Qnil);
//        staticpro (&mouse_syms);
//        wheel_syms = Fmake_vector (make_number (sizeof (lispy_wheel_names)
//                / sizeof (lispy_wheel_names[0])),
//                Qnil);
//        staticpro (&wheel_syms);
//
//        {
//            int i;
//            int len = sizeof (modifier_names) / sizeof (modifier_names[0]);
//
//            modifier_symbols = Fmake_vector (make_number (len), Qnil);
//            for (i = 0; i < len; i++)
//                if (modifier_names[i])
//                    XVECTOR (modifier_symbols)->contents[i] = intern_c_string (modifier_names[i]);
//            staticpro (&modifier_symbols);
//        }
//
//        recent_keys = Fmake_vector (make_number (NUM_RECENT_KEYS), Qnil);
//        staticpro (&recent_keys);
//
//        this_command_keys = Fmake_vector (make_number (40), Qnil);
//        staticpro (&this_command_keys);
//
//        raw_keybuf = Fmake_vector (make_number (30), Qnil);
//        staticpro (&raw_keybuf);
//
//        Qextended_command_history = intern_c_string ("extended-command-history");
//        Fset (Qextended_command_history, Qnil);
//        staticpro (&Qextended_command_history);
//
//        accent_key_syms = Qnil;
//        staticpro (&accent_key_syms);
//
//        func_key_syms = Qnil;
//        staticpro (&func_key_syms);
//
//        drag_n_drop_syms = Qnil;
//        staticpro (&drag_n_drop_syms);
//
//        unread_switch_frame = Qnil;
//        staticpro (&unread_switch_frame);
//
//        internal_last_event_frame = Qnil;
//        staticpro (&internal_last_event_frame);
//
//        read_key_sequence_cmd = Qnil;
//        staticpro (&read_key_sequence_cmd);
//
//        menu_bar_one_keymap_changed_items = Qnil;
//        staticpro (&menu_bar_one_keymap_changed_items);
//
//        menu_bar_items_vector = Qnil;
//        staticpro (&menu_bar_items_vector);
//
//        help_form_saved_window_configs = Qnil;
//        staticpro (&help_form_saved_window_configs);
//
//        defsubr (&Scurrent_idle_time);
//        defsubr (&Sevent_symbol_parse_modifiers);
//        defsubr (&Sevent_convert_list);
//
//        defsubr (&Sread_key_sequence);
//        defsubr (&Sread_key_sequence_vector);
//        defsubr (&Srecursive_edit);
//        #if defined (HAVE_MOUSE) || defined (HAVE_GPM)
//        defsubr (&Strack_mouse);
//        #endif
//        defsubr (&Sinput_pending_p);
//        defsubr (&Scommand_execute);
//        defsubr (&Srecent_keys);
//        defsubr (&Sthis_command_keys);
//        defsubr (&Sthis_command_keys_vector);
//        defsubr (&Sthis_single_command_keys);
//        defsubr (&Sthis_single_command_raw_keys);
//        defsubr (&Sreset_this_command_lengths);
//        defsubr (&Sclear_this_command_keys);
//        defsubr (&Ssuspend_emacs);
//        defsubr (&Sabort_recursive_edit);
//        defsubr (&Sexit_recursive_edit);
//        defsubr (&Srecursion_depth);
//        defsubr (&Stop_level);
//        defsubr (&Sdiscard_input);
//        defsubr (&Sopen_dribble_file);
//        defsubr (&Sset_input_interrupt_mode);
//        defsubr (&Sset_output_flow_control);
//        defsubr (&Sset_input_meta_mode);
//        defsubr (&Sset_quit_char);
//        defsubr (&Sset_input_mode);
//        defsubr (&Scurrent_input_mode);
//        defsubr (&Sexecute_extended_command);
//        defsubr (&Sposn_at_point);
//        defsubr (&Sposn_at_x_y);

        g.defineSymbol("last-command-event");
        g.defineSymbol("last-nonmenu-event");

        g.defineSymbol("last-input-event");
        g.defineSymbol("unread-command-events");
        g.defineSymbol("unread-command-char");  //note:  DEFVAR_INT
        g.defineSymbol("unread-post-input-method-events");
        g.defineSymbol("unread-input-method-events");
        g.defineSymbol("meta-prefix-char", new LispInteger(033));
        g.defineSymbol("last-command");           //DEFVAR_KBOARD
        g.defineSymbol("real-last-command");     //DEFVAR_KBOARD
        g.defineSymbol("last-repeatable-command");                 //DEFVAR_KBOARD
        g.defineSymbol("this-command");
        g.defineSymbol("this-command-keys-shift-translated");
        g.defineSymbol("this-original-command");
        g.defineSymbol("auto-save-interval", new LispInteger(300)); //DEFVAR_INT
        g.defineSymbol("auto-save-timeout", new LispInteger(30));
        g.defineSymbol("echo-keystrokes", new LispInteger(1));
        g.defineSymbol("polling-period", new LispInteger(2));   //DEFVAR_INT
        g.defineSymbol("double-click-time", new LispInteger(500));
        g.defineSymbol("double-click-fuzz", new LispInteger(3)); //DEFVAR_INT
        g.defineSymbol("inhibit-local-menu-bar-menus");  //DEFVAR_BOOL, 0 by default
        g.defineSymbol("num-input-keys", new LispInteger(0)); //DEFVAR_INT
        g.defineSymbol("num-nonmacro-input-events", new LispInteger(0));
        g.defineSymbol("last-event-frame");
        g.defineSymbol("tty-erase-char");  //todo This variable is set up in sysdep.c
        g.defineSymbol("help-char", new LispInteger(CharUtil.Ctl ('H')));
        g.defineSymbol("help-event-list");
        g.defineSymbol("help-form");
        g.defineSymbol("prefix-help-command");
        g.defineSymbol("top-level");
        g.defineSymbol("keyboard-translate-table"); //DEFVAR_KBOARD
        g.defineSymbol("cannot-suspend");  //DEFVAR_BOOL, 0 by default
        g.defineSymbol("menu-prompting");  //DEFVAR_BOOL, 1 by default
        g.defineSymbol("menu-prompt-more-char", new LispInteger(' '));
        g.defineSymbol("extra-keyboard-modifiers", new LispInteger(0)); //DEFVAR_INT
        g.defineSymbol("deactivate-mark");
        g.defineSymbol("command-hook-internal");
        g.defineSymbol("pre-command-hook");
        g.defineSymbol("post-command-hook");
        g.defineSymbol("lucid-menu-bar-dirty-flag");
        g.defineSymbol("menu-bar-final-items");
        g.defineSymbol("overriding-terminal-local-map");    //DEFVAR_KBOARD
        g.defineSymbol("overriding-local-map");
        g.defineSymbol("overriding-local-map-menu-flag");
        g.defineSymbol("special-event-map", LispList.cons(new LispString("keymap"), LispSymbol.ourNil));
        g.defineSymbol("track-mouse");
        g.defineSymbol("system-key-alist");  //DEFVAR_KBOARD
        g.defineSymbol("local-function-key-map");  //DEFVAR_KBOARD
        g.defineSymbol("input-decode-map");  //DEFVAR_KBOARD
        g.defineSymbol("function-key-map", BuiltinsKey.makeSparseKeymap(g, LispSymbol.ourNil));
        g.defineSymbol("key-translation-map", BuiltinsKey.makeSparseKeymap(g, LispSymbol.ourNil));
        g.defineSymbol("deferred-action-list");
        g.defineSymbol("deferred-action-function");
        g.defineSymbol("suggest-key-bindings", LispSymbol.ourT);
        g.defineSymbol("timer-list");
        g.defineSymbol("timer-idle-list");
        g.defineSymbol("input-method-function");
        g.defineSymbol("input-method-previous-message");
        g.defineSymbol("show-help-function");
        g.defineSymbol("disable-point-adjustment");
        g.defineSymbol("global-disable-point-adjustment");
        g.defineSymbol("minibuffer-message-timeout", new LispInteger(2));
        g.defineSymbol("throw-on-input");
        g.defineSymbol("command-error-function");
        g.defineSymbol("enable-disabled-menus-and-buttons");
//
//        #if 0
//        DEFVAR_LISP ("echo-area-clear-hook", ...,
//        doc: /* Normal hook run when clearing the echo area.  */);
//        #endif
//            Qecho_area_clear_hook = intern_c_string ("echo-area-clear-hook");
//        staticpro (&Qecho_area_clear_hook);
//        Fset (Qecho_area_clear_hook, Qnil);




//        /* Create the initial keyboard. */
//        initial_kboard = (KBOARD *) xmalloc (sizeof (KBOARD));
//        init_kboard (initial_kboard);
//        /* Vwindow_system is left at t for now.  */
//        initial_kboard->next_kboard = all_kboards;
//        all_kboards = initial_kboard;
    }

    //todo
    public static void keys_of_keyboard () {
//        BuiltinsKey.initial_define_key (BuiltinsKey.globalMap, CharUtil.Ctl ('Z'), "suspend-emacs");
//        BuiltinsKey.initial_define_key (control_x_map, CharUtil.Ctl ('Z'), "suspend-emacs");
//        BuiltinsKey.initial_define_key (meta_map, CharUtil.Ctl ('C'), "exit-recursive-edit");
//        BuiltinsKey.initial_define_key (BuiltinsKey.globalMap, CharUtil.Ctl (']'), "abort-recursive-edit");
//        BuiltinsKey.initial_define_key (meta_map, 'x', "execute-extended-command");
//
//        initial_define_lispy_key (Vspecial_event_map, "delete-frame", "handle-delete-frame");
//        initial_define_lispy_key (Vspecial_event_map, "ns-put-working-text", "ns-put-working-text");
//        initial_define_lispy_key (Vspecial_event_map, "ns-unput-working-text", "ns-unput-working-text");
//        initial_define_lispy_key (Vspecial_event_map, "iconify-frame", "ignore");
//        initial_define_lispy_key (Vspecial_event_map, "make-frame-visible", "ignore");
//        initial_define_lispy_key (Vspecial_event_map, "save-session", "handle-save-session");
//
////        #ifdef HAVE_DBUS
////        /* Define a special event which is raised for dbus callback
////      functions.  */
////        initial_define_lispy_key (Vspecial_event_map, "dbus-event",
////                "dbus-handle-event");
////        #endif
//
//        initial_define_lispy_key (Vspecial_event_map, "config-changed-event", "ignore");
    }

    
}
