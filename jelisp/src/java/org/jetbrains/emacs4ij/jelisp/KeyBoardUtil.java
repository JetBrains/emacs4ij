package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsList;

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
    public static LObject lispyModifierList(int modifiers) {
        LObject modifierList = LispSymbol.ourNil;
        for (int i = 0; (1 << i) <= modifiers && i < NUM_MOD_NAMES; i++)
            if ((modifiers & (1 << i)) != 0)
                modifierList = LispList.cons(modifier_symbols.getItem(i), modifierList);
        return modifierList;
    }

    public static LispList parseModifiers (LObject symbol) {
        if (symbol instanceof LispInteger)
            return LispList.list (new LispInteger(((LispInteger) symbol).keyToChar()),
                    new LispInteger (((LispInteger) symbol).getData() & CharUtil.CHAR_MODIFIER_MASK));
        else if (!(symbol instanceof LispSymbol))
            return LispList.list();
        return ((LispSymbol) symbol).parseModifiers();
    }

    public static LObject reorderModifiers (LObject symbol) {
        LispList parsed = parseModifiers (symbol);
        try {
            return applyModifiers (((LispInteger)BuiltinsList.car(parsed.cdr())).getData(), parsed.car());
        } catch (ClassCastException e) {
            throw new LispException("Internal error");
        }
    }

    private static LispSymbol applyModifiersUncached (int modifiers, String base) {
        /* Since BASE could contain nulls, we can't use intern here; we have
    to use Fintern, which expects a genuine Lisp_String, and keeps a
    reference to it.  */
        String new_mods =
        = (char *) alloca (sizeof ("A-C-H-M-S-s-down-drag-double-triple-"));
        int mod_len;

        {
            char *p = new_mods;

            /* Only the event queue may use the `up' modifier; it should always
be turned into a click or drag event before presented to lisp code.  */
            if (KeyBoardModifier.UP.andNotZero(modifiers))
                abort ();

            if (KeyBoardModifier.ALT.andNotZero(modifiers))   { new_mods += "A-"; }
            if (KeyBoardModifier.CTRL.andNotZero(modifiers))  { new_mods += "C-"; }
            if (KeyBoardModifier.HYPER.andNotZero(modifiers)) { new_mods += "H-"; }{ *p++ = 'H'; *p++ = '-'; }
            if (KeyBoardModifier.META.andNotZero(modifiers))  { new_mods += "C-"; }{ *p++ = 'M'; *p++ = '-'; }
            if (KeyBoardModifier.SHIFT.andNotZero(modifiers)) { new_mods += "C-"; }{ *p++ = 'S'; *p++ = '-'; }
            if (KeyBoardModifier.SUPER.andNotZero(modifiers)) { new_mods += "C-"; }{ *p++ = 's'; *p++ = '-'; }
            if (KeyBoardModifier.DOUBLE.andNotZero(modifiers))  { strcpy (p, "double-");  p += 7; }
            if (KeyBoardModifier.TRIPLE.andNotZero(modifiers))  { strcpy (p, "triple-");  p += 7; }
            if (KeyBoardModifier.DOWN.andNotZero(modifiers))  { strcpy (p, "down-");  p += 5; }
            if (KeyBoardModifier.DRAG.andNotZero(modifiers))  { strcpy (p, "drag-");  p += 5; }
            /* The click modifier is denoted by the absence of other modifiers.  */

            *p = '\0';

            mod_len = p - new_mods;
        }

        {
            LObject new_name;

            new_name = make_uninit_multibyte_string (mod_len + base_len,
                    mod_len + base_len_byte);
            bcopy (new_mods, SDATA (new_name),	       mod_len);
            bcopy (base,     SDATA (new_name) + mod_len, base_len_byte);

            return Fintern (new_name, Qnil);
        }
    }


    private static LObject applyModifiers (int modifiers, LObject base) {
        modifiers &= LispNumber.INTMASK;
        if (base instanceof LispInteger)
            return new LispInteger(((LispInteger) base).getData() | modifiers);
        if (!(base instanceof LispSymbol))
            throw new WrongTypeArgumentException("symbolp", base.toString());

        LObject cache = ((LispSymbol) base).getProperty("modifier-cache");
        LispInteger index = new LispInteger(modifiers & ~KeyBoardModifier.CLICK.value);
        LObject entry = BuiltinsCore.assqNoQuit(index, cache);

        LispSymbol newSymbol;
        if (entry instanceof LispList)
            newSymbol = (LispSymbol) ((LispList) entry).cdr();
        else {
            newSymbol = applyModifiersUncached(modifiers, ((LispSymbol) base).getName());
            entry = LispList.cons (index, newSymbol);
            ((LispSymbol) base).setProperty("modifier-cache", LispList.cons(entry, cache));
        }
        if (newSymbol.getProperty("event-kind").equals(LispSymbol.ourNil)) {
            LObject kind = ((LispSymbol) base).getProperty("event-kind");
            if (!kind.equals(LispSymbol.ourNil))
                newSymbol.setProperty("event-kind", kind);
        }
        return newSymbol;
    }

}
