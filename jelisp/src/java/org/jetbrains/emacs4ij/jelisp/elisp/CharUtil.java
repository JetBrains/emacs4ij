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


    /* Number of characters (in bits) each element of Nth level char-table covers.  */
    public static int[] CHARTABLE_BITS = {CHARTABLE_SIZE_BIT[1] + CHARTABLE_SIZE_BIT[2] + CHARTABLE_SIZE_BIT[3],
            CHARTABLE_SIZE_BIT[2] + CHARTABLE_SIZE_BIT[3],
            CHARTABLE_SIZE_BIT[3],
            0};


    public static int index (int c, int depth, int minChar) {
        return ((char)c - (char)minChar) >> CHARTABLE_BITS[depth];
    }

    public static int Ctl(int c) { 
        return 037 & c;
    }

    private static boolean CHAR_BYTE8_P (int c) {
        int MAX_5_BYTE_CHAR = 0x3FFF7F;
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

}
