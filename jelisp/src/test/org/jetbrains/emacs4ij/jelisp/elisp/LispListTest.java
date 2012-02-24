package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 10:28 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispListTest {

    @Test
    public void testCreateTrueList () {
        LispList list = LispList.list();
        Assert.assertEquals(LispSymbol.ourNil, list);
    }
    
    @Test
    public void testCdr () {
        LispList list = LispList.list(new LispSymbol("test"));
        LObject list1 = list.cdr();
        Assert.assertEquals(LispList.list(), list1);
    }
    
    @Test
    public void testToStringTrueList() {
        LispList list = LispList.list(new LispSymbol("test"));
        Assert.assertEquals("(test)", list.toString());
        list = LispList.list();
        Assert.assertEquals("nil", list.toString());
        list = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
        Assert.assertEquals("(1 2 3)", list.toString());
    }
    
    @Test
    public void testMemq() {
        LispList list = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
        LObject m = BuiltinsList.memq(new LispInteger(2), list);
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), m);
    }
    
    @Test
    public void testListOfNils () {
        LispList list = LispList.list(LispSymbol.ourNil, LispSymbol.ourNil);
        Assert.assertEquals("(nil nil)", list.toString());
        ArrayList<LObject> tList = new ArrayList<LObject> ();
        tList.add(LispSymbol.ourNil);
        tList.add(LispSymbol.ourNil);
        Assert.assertEquals(tList, list.toLObjectList());
    }

    @Test
    public void testtool() {
        LispList list = LispList.list(new LispInteger(1), new LispInteger(2));
        list.toString();
    }
    
    @Test
    public void testToObjectList() {
        LispList list = LispList.testList(LispList.list(new LispSymbol("a")), LispList.cons(LispSymbol.ourNil, LispList.list(new LispSymbol("b"))));
        List<LObject> a = list.toLObjectList();
        Assert.assertEquals(3, a.size());
    }
    
    @Test
    public void testAppend() {
        LispList a = LispList.list(new LispSymbol("a"));
        LispList b = LispList.list(new LispSymbol("b"));
        a.append(b);
        Assert.assertEquals(LispList.list(new LispSymbol("a"), new LispSymbol("b")), a);
    }
}
