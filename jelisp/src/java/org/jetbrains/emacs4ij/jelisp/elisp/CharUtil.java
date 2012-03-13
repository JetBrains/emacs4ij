package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.KeyBoardModifier;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/1/12
 * Time: 5:15 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class CharUtil {
    // these magic numbers are hardcoded in chartab.c, line 31
    public static final int[] CHARTABLE_SIZE_BIT = {6, 4, 5, 7};

    public static final int CHARACTERBITS = 22;

    public static final int MAX_CHAR   = 0x3FFFFF;

    public static final int CHAR_ALT   = 0x0400000;
    public static final int CHAR_SUPER = 0x0800000;
    public static final int CHAR_HYPER = 0x1000000;
    public static final int CHAR_SHIFT = 0x2000000;
    public static final int CHAR_CTL  = 0x4000000;
    public static final int CHAR_META = 0x8000000;

    public static final int CHAR_MODIFIER_MASK = (CHAR_ALT | CHAR_SUPER | CHAR_HYPER  | CHAR_SHIFT | CHAR_CTL | CHAR_META);

    public static final int MAX_N_EXTRA_SLOTS = 10;

    private static LispCharTable charUnifyTable = null;

    private static int MAX_1_BYTE_CHAR = 0x7F;
    private static int MAX_2_BYTE_CHAR = 0x7FF;
    private static int MAX_3_BYTE_CHAR = 0xFFFF;
    private static int MAX_4_BYTE_CHAR = 0x1FFFFF;
    private static int MAX_5_BYTE_CHAR = 0x3FFF7F;
    private static int MAX_UNICODE_CHAR = 0x10FFFF;

    /* Number of characters (in bits) each element of Nth level char-table covers.  */
    public static int[] CHARTABLE_BITS = {CHARTABLE_SIZE_BIT[1] + CHARTABLE_SIZE_BIT[2] + CHARTABLE_SIZE_BIT[3],
            CHARTABLE_SIZE_BIT[2] + CHARTABLE_SIZE_BIT[3],
            CHARTABLE_SIZE_BIT[3],
            0};

    /* Number of elements in Nth level char-table.  */
    public static int charTableSize(int i) {
        return 1 << CHARTABLE_SIZE_BIT[i];
    }

    /* Number of characters each element of Nth level char-table covers.  */
    public static int charTableChars(int i) {
        return 1 << CHARTABLE_BITS[i];
    }

    public static int index (int c, int depth, int minChar) {
        return ((char)c - (char)minChar) >> CHARTABLE_BITS[depth];
    }

    public static int Ctl(int c) { 
        return 037 & c;
    }

    public static boolean isAsciiChar (int c) {
        return (char)c < 0x80;
    }

    /* Apply the control modifier to CHARACTER.  */
    public static int makeCtrlChar (int c) {
        int upper = c & ~0177;  /* Save the upper bits here.  */
        if (! isAsciiChar (c))
            return KeyBoardModifier.CTRL.bitwiseOr(c);

        c &= 0177;
        /* Everything in the columns containing the upper-case letters denotes a control character.  */
        if (c >= 0100 && c < 0140) {
            int oc = c;
            c &= ~0140;
            /* Set the shift modifier for a control char made from a shifted letter.  But only for letters!  */
            if (oc >= 'A' && oc <= 'Z')
                c = KeyBoardModifier.SHIFT.bitwiseOr(c);
        } else /* The lower-case letters denote control characters too.  */
            if (c >= 'a' && c <= 'z')
                c &= ~0140;
        else /* Include the bits for control and shift only if the basic ASCII code can't indicate them.  */
            if (c >= ' ')
                c = KeyBoardModifier.CTRL.bitwiseOr(c);
        /* Replace the high bits.  */
        c |= KeyBoardModifier.CTRL.bitwiseAndNot(upper);
        return c;
    }

    private static boolean isSingleByteChar (int c) {
        return c < 0x100;
    }
    
    public static String pushKeyDescription (int c, boolean forceMultiByte) {
        /* Clear all the meaningless bits above the meta bit.  */
        c &= KeyBoardModifier.metaValue() | ~ -KeyBoardModifier.metaValue();
        int c2 = c & ~KeyBoardModifier.bitwiseOr(KeyBoardModifier.ALT, KeyBoardModifier.CTRL, KeyBoardModifier.HYPER,
                KeyBoardModifier.META, KeyBoardModifier.SHIFT, KeyBoardModifier.SUPER);
        
        if (!BuiltinPredicates.isCharacter(new LispInteger(c2))) {
            return "[" + (char)c + ']';
        }
        String p = "";
        if (KeyBoardModifier.ALT.andNotZero(c)) {
            p += "A-";
            c = KeyBoardModifier.ALT.minus(c);
        }
        if (KeyBoardModifier.CTRL.andNotZero(c) || (c2 < ' ' && c2 != 27 && c2 != '\t' && c2 != Ctl ('M'))) {
            p += "C-";
            c = KeyBoardModifier.CTRL.bitwiseAndNot(c);
        }
        if (KeyBoardModifier.HYPER.andNotZero(c)) {
            p += "H-";
            c = KeyBoardModifier.HYPER.minus(c);
        }
        if (KeyBoardModifier.META.andNotZero(c)) {
            p += "M-";
            c = KeyBoardModifier.META.minus(c);
        }
        if (KeyBoardModifier.SHIFT.andNotZero(c)) {
            p += "S-";
            c = KeyBoardModifier.SHIFT.minus(c);
        }
        if (KeyBoardModifier.SUPER.andNotZero(c)) {
            p += "s-";
            c = KeyBoardModifier.SUPER.minus(c);
        }

        boolean areMultibyteCharactersDisabled = true;          
        LispSymbol s = GlobalEnvironment.INSTANCE.find("enable-multibyte-characters");
        if (s != null)
            areMultibyteCharactersDisabled = s.getValue().equals(LispSymbol.ourNil); 
        
        if (c < 040) {
            if (c == 033) {
                p += "ESC";
            } else if (c == '\t') {
                p += "TAB";
            } else if (c == Ctl ('M')) {
                p += "RET";
            } else { /* `C-' already added above.  */
                if (c > 0 && c <= Ctl ('Z'))
                    p += (char) (c + 0140);
                else
                    p += (char) (c + 0100);
            }
        } else if (c == 0177) {
            p += "DEL";
        } else if (c == ' ') {
            p += "SPC";
        }
        else if (c < 128 || (areMultibyteCharactersDisabled && isSingleByteChar(c) && !forceMultiByte)) {
            p += (char)c;
        } else { /* Now we are sure that C is a valid character code.  */
            if (areMultibyteCharactersDisabled && !forceMultiByte)
                p += (char)multibyteCharToUnibyte(c);
            else
                p += CHAR_STRING(c);
        }

        return p;
    }

    private static String CHAR_STRING(int c) {
        return c <= MAX_1_BYTE_CHAR ? new String(new char[] {(char)c}) 
                : c <= MAX_2_BYTE_CHAR ? new String(new char[] {(char)(0xC0 | ((c) >> 6)), (char)(0x80 | ((c) & 0x3F))})
                : c <= MAX_3_BYTE_CHAR ? new String(new char[] {(char)(0xE0 | ((c) >> 12)), (char)(0x80 | (((c) >> 6) & 0x3F)), (char)(0x80 | ((c) & 0x3F))})
                : char_string (c);
    }

    private static String char_string (int c) {
        if ((c & CHAR_MODIFIER_MASK) != 0) {
            c = char_resolve_modifier_mask ((int) c);
            /* If c still has any modifier bits, just ignore it.  */
            c &= ~CHAR_MODIFIER_MASK;
        }
        int cc = MAYBE_UNIFY_CHAR (c);
        if (cc != -1)
            c = cc;
        if (c <= MAX_3_BYTE_CHAR) {
            return CHAR_STRING (c);
        } else if (c <= MAX_4_BYTE_CHAR) {
            return new String(new char[] {(char) (0xF0 | (c >> 18)),
                                       (char) (0x80 | ((c >> 12) & 0x3F)),
                                       (char) (0x80 | ((c >> 6) & 0x3F)),
                                       (char) (0x80 | (c & 0x3F))}); 
        }
        else if (c <= MAX_5_BYTE_CHAR) {
            return new String(new char[] {(char) 0xF8,
                    (char) (0x80 | ((c >> 18) & 0x0F)),
                    (char) (0x80 | ((c >> 6) & 0x3F)),
                    (char) (0x80 | (c & 0x3F)),
                    (char) (0x80 | (c & 0x3F))});
        } else if (c <= MAX_CHAR) {
            c = CHAR_TO_BYTE8 (c);
            return BYTE8_STRING (c);
        }
        else
            BuiltinsCore.error (GlobalEnvironment.INSTANCE, "Invalid character: " + c);

        return null;
    }

    private static String BYTE8_STRING(int b) {
        return new String(new char[] {(char)(0xC0 | (((b) >> 6) & 0x01)), (char)(0x80 | ((b) & 0x3F))});
    }
    
    private static boolean CHAR_BYTE8_P (int c) {
        return c > MAX_5_BYTE_CHAR;
    }

    private static int CHAR_TO_BYTE8(int c) {	
        return CHAR_BYTE8_P (c)
            ? c - 0x3FFF00
            : multibyteCharToUnibyte (c);
    }
    
    private static int multibyteCharToUnibyte (int c) {
        if (c < 0x80)
            return c;
        if (CHAR_BYTE8_P (c))
            return CHAR_TO_BYTE8 (c);
        return (c & 0xFF);
    }

    private static int char_resolve_modifier_mask (int c) {
        /* A non-ASCII character can't reflect modifier bits to the code.  */
        if (! isAsciiChar((c & ~CHAR_MODIFIER_MASK)))
            return c;

        /* For Meta, Shift, and Control modifiers, we need special care.  */
        if ((c & CHAR_SHIFT) != 0) {
            /* Shift modifier is valid only with [A-Za-z].  */
            if ((c & 0377) >= 'A' && (c & 0377) <= 'Z')
                c &= ~CHAR_SHIFT;
            else if ((c & 0377) >= 'a' && (c & 0377) <= 'z')
                c = (c & ~CHAR_SHIFT) - ('a' - 'A');
                /* Shift modifier for control characters and SPC is ignored.  */
            else if ((c & ~CHAR_MODIFIER_MASK) <= 0x20)
                c &= ~CHAR_SHIFT;
        }
        if ((c & CHAR_CTL) != 0) {
            /* Simulate the code in lread.c.=)  */
            /* Allow `\C- ' and `\C-?'.  */
            if ((c & 0377) == ' ')
                c &= ~0177 & ~ CHAR_CTL;
            else if ((c & 0377) == '?')
                c = 0177 | (c & ~0177 & ~CHAR_CTL);
                /* ASCII control chars are made from letters (both cases),
         as well as the non-letters within 0100...0137.  */
            else if ((c & 0137) >= 0101 && (c & 0137) <= 0132)
                c &= (037 | (~0177 & ~CHAR_CTL));
            else if ((c & 0177) >= 0100 && (c & 0177) <= 0137)
                c &= (037 | (~0177 & ~CHAR_CTL));
        }
        return c;
    }

    private static int MAYBE_UNIFY_CHAR(int c) {
        if (c > MAX_UNICODE_CHAR && c <= MAX_5_BYTE_CHAR) {
            LObject val = charUnifyTable == null ? LispSymbol.ourNil : charUnifyTable.charTableRef(c);
            if (val instanceof LispInteger)
                return ((LispInteger) val).getData();
            else if (!val.equals(LispSymbol.ourNil))
                return -1;
                //todo return maybe_unify_char (c, val);
            
        }
        return -1;
    }


}
