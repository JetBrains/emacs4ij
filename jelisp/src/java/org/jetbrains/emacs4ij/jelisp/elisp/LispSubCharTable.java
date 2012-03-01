package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.Arrays;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/28/12
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispSubCharTable extends LispObject implements LispArray {
    private int myDepth; // == [1, 2, 3]
    private int myMinChar;
    private LObject[] myContent;

    public LispSubCharTable(int depth, int minChar, LObject init) {
        myDepth = depth;
        myMinChar = (char)minChar;
        myContent = new LObject[CharTableUtil.MIN_CHARTABLE_SIZE - 1 + (1 << CharTableUtil.CHARTABLE_SIZE_BIT[depth])];
        Arrays.fill(myContent, init);
    }

    @Override
    public LObject evaluate(Environment environment) {
        return null;
    }

    public void set (int c, LObject value) {
        int i = CharTableUtil.index(c, myDepth, myMinChar);
        if (myDepth == 3)
            myContent[i] = value;
        else {
            if (!(myContent[i] instanceof LispSubCharTable)) {
                myContent[i] = new LispSubCharTable (myDepth + 1,
                        myMinChar + i * CharTableUtil.charTableChar(myDepth), myContent[i]);
            }
            ((LispSubCharTable)myContent[i]).set(c, value);
        }
    }

    @Override
    public String toString() {
        //todo
        return "^sub-char-t";
    }

    @Override
    public void setItem(int position, LObject value) {
        myContent[position] = value;
    }

    @Override
    public LObject getItem(int position) {
        return myContent[position];
    }

    public static LObject setRange (LObject table, int depth, int minChar, int from, int to, LObject value) {
        int maxChar = minChar + CharTableUtil.charTableChar(depth) - 1;
        if (depth == 3 || (from <= minChar && to >= maxChar)) {
            return value;
        }
        depth++;
        if (!(table instanceof LispSubCharTable))
            table = new LispSubCharTable(depth, minChar, table);
        if (from < minChar)
            from = minChar;
        if (to > maxChar)
            to = maxChar;
        int i = CharTableUtil.index(from, depth, minChar);
        int j = CharTableUtil.index(to, depth, minChar);
        minChar += CharTableUtil.charTableChar(depth) * i;
        for (; i <= j; i++, minChar += CharTableUtil.charTableChar(depth)) {
            LispSubCharTable subTable = (LispSubCharTable) table;
            LObject tmp = setRange (subTable.getItem(i), depth, minChar, from, to, value);
            if (tmp != null)
                subTable.setItem(i, tmp);
        }
        return null;
    }
}
