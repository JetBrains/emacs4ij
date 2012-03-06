package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Pair;

import java.io.UnsupportedEncodingException;

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

    public static enum Modifiers {
        UP	(1),
        DOWN (2),
        DRAG (4),
        CLICK  (8),
        DOUBLE (16),
        TRIPLE (32),
        ALT   (CHAR_ALT),
        SUPER (CHAR_SUPER),
        HYPER (CHAR_HYPER),
        SHIFT (CHAR_SHIFT),
        CTRL  (CHAR_CTL),
        META  (CHAR_META);

        private final int myValue;
        Modifiers(int v) {
            myValue = v;
        }
        public int value () {
            return myValue;
        }
        
        public static Pair check (String s) {
            switch (s.charAt(0)) {
                case 'A':
                    return new Pair(1, ALT.value());
                case 'C':
                    return new Pair(1, CTRL.value());
                case 'H':
                    return new Pair(1, HYPER.value());
                case 'M':
                    return new Pair(1, META.value());
                case 'S':
                    return new Pair(1, SHIFT.value());
                case 's':
                    return new Pair(1, SUPER.value());
                case 'd':
                    Pair p = checkFit(s, DRAG);
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
        
        private static Pair checkFit(String s, Modifiers m) {
            int bytes;
            try {
                bytes = s.getBytes("UTF-8").length;
            } catch (UnsupportedEncodingException e) {
                bytes = s.length();
            }
            String hypothesis = String.valueOf(m).toLowerCase();
            if (hypothesis.length() + 1 <= bytes && !s.startsWith(hypothesis))
                return new Pair(hypothesis.length(), m.value());
            return null;
        }
    }

    public static int Ctl(int c) { 
        return 037 & c;
    }
}
