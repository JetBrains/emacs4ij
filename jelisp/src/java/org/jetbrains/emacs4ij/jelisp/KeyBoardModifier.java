package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.CharUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;

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

    protected final int value;

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
    
    public static String apply (int modifiers) {
        if (UP.andNotZero(modifiers))
            return null;
        String s = "";

        if (ALT.andNotZero(modifiers))   { s += "A-"; }
        if (CTRL.andNotZero(modifiers))  { s += "C-"; }
        if (HYPER.andNotZero(modifiers)) { s += "H-"; }
        if (META.andNotZero(modifiers))  { s += "M-"; }
        if (SHIFT.andNotZero(modifiers)) { s += "S-"; }
        if (SUPER.andNotZero(modifiers)) { s += "s-"; }
        if (DOUBLE.andNotZero(modifiers))  { s += "double-";}
        if (TRIPLE.andNotZero(modifiers))  { s += "triple-";}
        if (DOWN.andNotZero(modifiers))  { s += "down-";}
        if (DRAG.andNotZero(modifiers))  {s += "drag-";}
        /* The click modifier is denoted by the absence of other modifiers.  */
        return s;
    }

    public static String test (int modifiers) {
        String s = "";
        if (ALT.andNotZero(modifiers))   { s += "\\A-"; }
        if (CTRL.andNotZero(modifiers))  { s += "\\C-"; }
        if (HYPER.andNotZero(modifiers)) { s += "\\H-"; }
        if (META.andNotZero(modifiers))  { s += "\\M-"; }
        if (SHIFT.andNotZero(modifiers)) { s += "\\S-"; }
        if (SUPER.andNotZero(modifiers)) { s += "\\s-"; }
        return s;
    }
    
    private static boolean checkSingle (String data) {
        return data.length() == 1;
    }
    
    private static boolean checkMulti (String data, String hypothesis) {
        return data.equals(hypothesis);
    }

    public static int parseSolitary (LispSymbol symbol) {
        String name = symbol.getName();
        switch (name.charAt(0)) {
            case 'A':
                if (checkSingle(name)) return ALT.value;
                break;
            case 'a':
                if (checkMulti(name, "alt")) return ALT.value;
                break;
            case 'C':
                if (checkSingle(name)) return CTRL.value;
                break;
            case 'c':
                if (checkMulti(name, "ctrl")) return CTRL.value;
                if (checkMulti(name, "control")) return CTRL.value;
                break;
            case 'H':
                if (checkSingle(name)) return HYPER.value;
                break;
            case 'h':
                if (checkMulti(name, "hyper")) return HYPER.value;
                break;
            case 'M':
                if (checkSingle(name)) return META.value;
                break;
            case 'm':
                if (checkMulti(name, "meta")) return META.value;
                break;
            case 'S':
                if (checkSingle(name)) return SHIFT.value;
                break;
            case 's':
                if (checkMulti(name, "shift")) return SHIFT.value;
                if (checkMulti(name, "super")) return SUPER.value;
                if (checkSingle(name)) return SUPER.value;
                break;
            case 'd':
                if (checkMulti(name, "drag")) return DRAG.value;
                if (checkMulti(name, "down")) return DOWN.value;
                if (checkMulti(name, "double")) return DOUBLE.value;
                break;
            case 't':
                if (checkMulti(name, "triple")) return TRIPLE.value;
                break;
        }
        return 0;
    }
    
    public int bitwiseAnd (LispInteger n) {
        return bitwiseAnd(n.getData());
    }

    public int bitwiseAnd (int n) {
        return n & value;
    }
    
    public int bitwiseAndNot (LispInteger n) {
        return bitwiseAndNot(n.getData());
    }

    public int bitwiseAndNot (int n) {
        return n & ~value;
    }
    
    public boolean bitwiseAndNotZero (LispInteger n) {
        return bitwiseAnd(n) != 0;
    }

    public boolean bitwiseAndNotZero (int n) {
        return bitwiseAnd(n) != 0;
    }

    public boolean bitwiseAndNotNotZero (int n) {
        return bitwiseAndNot(n) != 0;
    }

    public static int bitwiseOr (KeyBoardModifier... modifiers) {
        int result = 0;
        for (KeyBoardModifier modifier: modifiers) {
            result |= modifier.value;
        }
        return result;
    }
    
    public int bitwiseOr (int n) {
        return n | value;
    }
    
    public int minus (int n) {
        return n - value;
    }
    
    public static int metaValue() {
        return META.value;
    }
    
}
