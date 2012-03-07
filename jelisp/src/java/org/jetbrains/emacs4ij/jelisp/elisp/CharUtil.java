package org.jetbrains.emacs4ij.jelisp.elisp;

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

    public static int Ctl(int c) { 
        return 037 & c;
    }
}
