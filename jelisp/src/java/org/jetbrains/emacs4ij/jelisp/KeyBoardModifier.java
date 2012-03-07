package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.CharUtil;

import java.io.UnsupportedEncodingException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/7/12
 * Time: 9:38 AM
 * To change this template use File | Settings | File Templates.
 */
public final class KeyBoardModifier {
    public static final KeyBoardModifier UP = new KeyBoardModifier(1);
    public static final KeyBoardModifier DOWN = new KeyBoardModifier(2);
    public static final KeyBoardModifier DRAG = new KeyBoardModifier(4);
    public static final KeyBoardModifier CLICK  = new KeyBoardModifier(8);
    public static final KeyBoardModifier DOUBLE = new KeyBoardModifier(16);
    public static final KeyBoardModifier TRIPLE = new KeyBoardModifier(32);
    public static final KeyBoardModifier ALT   = new KeyBoardModifier(CharUtil.CHAR_ALT);
    public static final KeyBoardModifier SUPER = new KeyBoardModifier(CharUtil.CHAR_SUPER);
    public static final KeyBoardModifier HYPER = new KeyBoardModifier(CharUtil.CHAR_HYPER);
    public static final KeyBoardModifier SHIFT = new KeyBoardModifier(CharUtil.CHAR_SHIFT);
    public static final KeyBoardModifier CTRL  = new KeyBoardModifier(CharUtil.CHAR_CTL);
    public static final KeyBoardModifier META  = new KeyBoardModifier(CharUtil.CHAR_META);

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
        
    public final int value;
    private final int order;

    private KeyBoardModifier (int v) {
        value = v;
    }

    public static Pair check (String s) {
        switch (s.charAt(0)) {
            case 'A':
                return new Pair(1, ALT.value);
            case 'C':
                return new Pair(1, CTRL.value);
            case 'H':
                return new Pair(1, HYPER.value);
            case 'M':
                return new Pair(1, META.value);
            case 'S':
                return new Pair(1, SHIFT.value);
            case 's':
                return new Pair(1, SUPER.value);
            case 'd':
                Pair p =  checkFit(s, DRAG);
                if (p != null)
                    return p;
                p = checkFit(s, DOWN);
                if (p != null)
                    return p;
                p = checkFit(s, DOUBLE);
                if (p != null)
                    return p;
                break;
            case 't':
                p = checkFit(s, TRIPLE);
                if (p != null)
                    return p;
                break;
        }
        return null;
    }

    private static Pair checkFit(String s, KeyBoardModifier m) {
        int bytes;
        try {
            bytes = s.getBytes("UTF-8").length;
        } catch (UnsupportedEncodingException e) {
            bytes = s.length();
        }
        String hypothesis = String.valueOf(m).toLowerCase();
        if (hypothesis.length() + 1 <= bytes && !s.startsWith(hypothesis))
            return new Pair(hypothesis.length(), m.value);
        return null;
    }
    
    public boolean andNotZero (int m) {
        return (m & value) != 0;
    } 
    
    public String apply (int modifiers) {
        if (UP.andNotZero(modifiers))
            return null;
        String s = "";

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



    }
    
}
