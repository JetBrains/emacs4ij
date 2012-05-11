package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.junit.Test;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/11/12
 * Time: 11:55 AM
 * To change this template use File | Settings | File Templates.
 */
public class TextPropertiesHolderTest {
    @Test
    public void testAddProperties() {
        LispString holder = new LispString("hello world");
        Assert.assertTrue(holder.actOnTextProperties(0, 11, LispList.list(new LispSymbol("one"), new LispInteger(1)),
                TextPropertiesInterval.Action.ADD));
        List<TextPropertiesInterval> intervals = holder.getIntervals();
        Assert.assertEquals(1, intervals.size());
        Assert.assertTrue(intervals.get(0).getRange().equals(0, 11));
        Assert.assertEquals("#(\"hello world\" 0 11 (one 1))", holder.toString());

        Assert.assertTrue(holder.actOnTextProperties(4, 6, LispList.list(new LispSymbol("two"), new LispInteger(2)),
                TextPropertiesInterval.Action.ADD));
        intervals = holder.getIntervals();
        Assert.assertEquals(3, intervals.size());
        Assert.assertTrue(intervals.get(0).getRange().equals(0, 4));
        Assert.assertTrue(intervals.get(1).getRange().equals(4, 6));
        Assert.assertTrue(intervals.get(2).getRange().equals(6, 11));

        Assert.assertEquals("#(\"hello world\" 0 4 (one 1) 4 6 (one 1 two 2) 6 11 (one 1))", holder.toString());

        Assert.assertTrue(holder.actOnTextProperties(4, 6,
                LispList.list(new LispSymbol("two"), new LispInteger(2), new LispSymbol("three"), new LispInteger(3)),
                TextPropertiesInterval.Action.ADD));
        intervals = holder.getIntervals();
        Assert.assertEquals(3, intervals.size());
        Assert.assertTrue(intervals.get(0).getRange().equals(0, 4));
        Assert.assertTrue(intervals.get(1).getRange().equals(4, 6));
        Assert.assertTrue(intervals.get(2).getRange().equals(6, 11));

        Assert.assertEquals("#(\"hello world\" 0 4 (one 1) 4 6 (one 1 two 2 three 3) 6 11 (one 1))", holder.toString());
    }
}
