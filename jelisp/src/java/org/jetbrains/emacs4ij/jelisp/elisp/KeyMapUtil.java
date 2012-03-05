package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsKey;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/5/12
 * Time: 3:28 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class KeyMapUtil {
    public static void defineKeyMaps (GlobalEnvironment g) {
//        staticpro (&Qkeymap);
        staticpro (&apropos_predicate);
        staticpro (&apropos_accumulate);
        apropos_predicate = LispSymbol.ourNil;
        apropos_accumulate = LispSymbol.ourNil;

        Qkeymap_canonicalize = intern_c_string ("keymap-canonicalize");
        staticpro (&Qkeymap_canonicalize);

        /* Now we are ready to set up this property, so we can
    create char tables.  */
        LispSymbol k = new LispSymbol("keymap");
        k.setProperty("char-table-extra-slots", new LispInteger(0));
        g.defineSymbol(k);


        /* Initialize the keymaps standardly used.
  Each one is the value of a Lisp variable, and is also
  pointed to by a C variable */

        LObject global_map = BuiltinsKey.makeKeymap(LispSymbol.ourNil);
        BuiltinsCore.set(g, new LispSymbol("global-map"), global_map);

        LObject current_global_map = global_map;
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



        DEFVAR_LISP ("define-key-rebound-commands", &Vdefine_key_rebound_commands,
                doc: /* List of commands given new key bindings recently.
This is used for internal purposes during Emacs startup;
don't alter it yourself.  */);
        Vdefine_key_rebound_commands = Qt;

        DEFVAR_LISP ("minibuffer-local-map", &Vminibuffer_local_map,
                doc: /* Default keymap to use when reading from the minibuffer.  */);
        Vminibuffer_local_map = Fmake_sparse_keymap (LispSymbol.ourNil);

        DEFVAR_LISP ("minibuffer-local-ns-map", &Vminibuffer_local_ns_map,
                doc: /* Local keymap for the minibuffer when spaces are not allowed.  */);
        Vminibuffer_local_ns_map = Fmake_sparse_keymap (LispSymbol.ourNil);
        Fset_keymap_parent (Vminibuffer_local_ns_map, Vminibuffer_local_map);

        DEFVAR_LISP ("minibuffer-local-completion-map", &Vminibuffer_local_completion_map,
                doc: /* Local keymap for minibuffer input with completion.  */);
        Vminibuffer_local_completion_map = Fmake_sparse_keymap (LispSymbol.ourNil);
        Fset_keymap_parent (Vminibuffer_local_completion_map, Vminibuffer_local_map);

        DEFVAR_LISP ("minibuffer-local-filename-completion-map",
                &Vminibuffer_local_filename_completion_map,
                doc: /* Local keymap for minibuffer input with completion for filenames.  */);
        Vminibuffer_local_filename_completion_map = Fmake_sparse_keymap (LispSymbol.ourNil);
        Fset_keymap_parent (Vminibuffer_local_filename_completion_map,
                Vminibuffer_local_completion_map);


        DEFVAR_LISP ("minibuffer-local-must-match-map", &Vminibuffer_local_must_match_map,
                doc: /* Local keymap for minibuffer input with completion, for exact match.  */);
        Vminibuffer_local_must_match_map = Fmake_sparse_keymap (LispSymbol.ourNil);
        Fset_keymap_parent (Vminibuffer_local_must_match_map,
                Vminibuffer_local_completion_map);

        DEFVAR_LISP ("minibuffer-local-filename-must-match-map",
                &Vminibuffer_local_filename_must_match_map,
                doc: /* Local keymap for minibuffer input with completion for filenames with exact match.  */);
        Vminibuffer_local_filename_must_match_map = Fmake_sparse_keymap (LispSymbol.ourNil);
        Fset_keymap_parent (Vminibuffer_local_filename_must_match_map,
                Vminibuffer_local_must_match_map);

        DEFVAR_LISP ("minor-mode-map-alist", &Vminor_mode_map_alist,
                doc: /* Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings if VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.  */);
        Vminor_mode_map_alist = LispSymbol.ourNil;

        DEFVAR_LISP ("minor-mode-overriding-map-alist", &Vminor_mode_overriding_map_alist,
                doc: /* Alist of keymaps to use for minor modes, in current major mode.
This variable is an alist just like `minor-mode-map-alist', and it is
used the same way (and before `minor-mode-map-alist'); however,
it is provided for major modes to bind locally.  */);
        Vminor_mode_overriding_map_alist = LispSymbol.ourNil;

        DEFVAR_LISP ("emulation-mode-map-alists", &Vemulation_mode_map_alists,
                doc: /* List of keymap alists to use for emulations modes.
It is intended for modes or packages using multiple minor-mode keymaps.
Each element is a keymap alist just like `minor-mode-map-alist', or a
symbol with a variable binding which is a keymap alist, and it is used
the same way.  The "active" keymaps in each alist are used before
`minor-mode-map-alist' and `minor-mode-overriding-map-alist'.  */);
        Vemulation_mode_map_alists = LispSymbol.ourNil;

        DEFVAR_LISP ("where-is-preferred-modifier", &Vwhere_is_preferred_modifier,
                doc: /* Preferred modifier to use for `where-is'.
When a single binding is requested, `where-is' will return one that
uses this modifier if possible.  If nil, or if no such binding exists,
bindings using keys without modifiers (or only with meta) will be
preferred.  */);
        Vwhere_is_preferred_modifier = LispSymbol.ourNil;
        where_is_preferred_modifier = 0;

        staticpro (&Vmouse_events);
        Vmouse_events = LispList.cons(intern_c_string ("menu-bar"),
                LispList.cons(intern_c_string ("tool-bar"),
                        LispList.cons(intern_c_string ("header-line"),
                                LispList.cons(intern_c_string ("mode-line"),
                                        LispList.cons(intern_c_string ("mouse-1"),
                                                LispList.cons(intern_c_string ("mouse-2"),
                                                        LispList.cons(intern_c_string ("mouse-3"),
                                                                LispList.cons(intern_c_string ("mouse-4"),
                                                                        LispList.cons(intern_c_string ("mouse-5"),
                                                                                LispSymbol.ourNil)))))))));


        Qsingle_key_description = intern_c_string ("single-key-description");
        staticpro (&Qsingle_key_description);

        Qkey_description = intern_c_string ("key-description");
        staticpro (&Qkey_description);

//        Qkeymapp = intern_c_string ("keymapp");
//        staticpro (&Qkeymapp);

        Qnon_ascii = intern_c_string ("non-ascii");
        staticpro (&Qnon_ascii);

        Qmenu_item = intern_c_string ("menu-item");
        staticpro (&Qmenu_item);

        Qremap = intern_c_string ("remap");
        staticpro (&Qremap);

        QCadvertised_binding = intern_c_string (":advertised-binding");
        staticpro (&QCadvertised_binding);

        command_remapping_vector = Fmake_vector (make_number (2), Qremap);
        staticpro (&command_remapping_vector);

        where_is_cache_keymaps = Qt;
        where_is_cache = LispSymbol.ourNil;
        staticpro (&where_is_cache);
        staticpro (&where_is_cache_keymaps);

        defsubr (&Skeymapp);
        defsubr (&Skeymap_parent);
        defsubr (&Skeymap_prompt);
        defsubr (&Sset_keymap_parent);
        defsubr (&Smake_keymap);
        defsubr (&Smake_sparse_keymap);
        defsubr (&Smap_keymap_internal);
        defsubr (&Smap_keymap);
        defsubr (&Scopy_keymap);
        defsubr (&Scommand_remapping);
        defsubr (&Skey_binding);
        defsubr (&Slocal_key_binding);
        defsubr (&Sglobal_key_binding);
        defsubr (&Sminor_mode_key_binding);
        defsubr (&Sdefine_key);
        defsubr (&Slookup_key);
        defsubr (&Sdefine_prefix_command);
        defsubr (&Suse_global_map);
        defsubr (&Suse_local_map);
        defsubr (&Scurrent_local_map);
        defsubr (&Scurrent_global_map);
        defsubr (&Scurrent_minor_mode_maps);
        defsubr (&Scurrent_active_maps);
        defsubr (&Saccessible_keymaps);
        defsubr (&Skey_description);
        defsubr (&Sdescribe_vector);
        defsubr (&Ssingle_key_description);
        defsubr (&Stext_char_description);
        defsubr (&Swhere_is_internal);
        defsubr (&Sdescribe_buffer_bindings);
        defsubr (&Sapropos_internal);
    }

    void keys_of_keymap ()
    {
        initial_define_key (global_map, 033, "ESC-prefix");
        initial_define_key (global_map, Ctl ('X'), "Control-X-prefix");
    }
}
