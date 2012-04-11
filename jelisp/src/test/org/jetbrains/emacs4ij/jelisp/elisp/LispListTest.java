package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.subroutine.BList;
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
        LispObject list1 = list.cdr();
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
    public void testToStringCons() {
        LispList list = LispList.cons(new LispInteger(1), LispSymbol.ourNil);
        Assert.assertEquals("(1)", list.toString());
        list = LispList.cons(LispSymbol.ourNil, new LispInteger(1));
        Assert.assertEquals("(nil . 1)", list.toString());
        list = LispList.cons(LispSymbol.ourNil, LispSymbol.ourNil);
        Assert.assertEquals("(nil)", list.toString());
    }

    @Test
    public void testToString() {
        LispList list = LispList.list(LispList.list(new LispInteger(1), new LispInteger(2)), LispSymbol.ourNil);
        Assert.assertEquals("((1 2) nil)", list.toString());
    }

    @Test
    public void testMemq() {
        LispList list = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
        LispObject m = BList.memq(new LispInteger(2), list);
        Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), m);
    }

    @Test
    public void testListOfNils () {
        LispList list = LispList.list(LispSymbol.ourNil, LispSymbol.ourNil);
        Assert.assertEquals("(nil nil)", list.toString());
        ArrayList<LispObject> tList = new ArrayList<LispObject> ();
        tList.add(LispSymbol.ourNil);
        tList.add(LispSymbol.ourNil);
        Assert.assertEquals(tList, list.toLispObjectList());
    }

    @Test
    public void testtool() {
        LispList list = LispList.list(new LispInteger(1), new LispInteger(2));
        list.toString();
    }

    @Test
    public void testToObjectList() {
        LispList list = LispList.list(LispList.list(new LispSymbol("a")), LispList.cons(LispSymbol.ourNil, LispList.list(new LispSymbol("b"))));
        List<LispObject> a = list.toLispObjectList();
        Assert.assertEquals(2, a.size());

        list = LispList.list(new LispInteger(1), LispList.cons(new LispInteger(2), new LispInteger(3)), new LispInteger(4));
        a = list.toLispObjectList();
        Assert.assertEquals(3, a.size());

        list = LispList.list(new LispSymbol("a"), LispList.list(new LispSymbol("b"), new LispSymbol("c")));
        a = list.toLispObjectList();
        Assert.assertEquals(2, a.size());
        Assert.assertTrue(a.get(1) instanceof LispList);

        list = LispList.list(new LispSymbol("a"), LispList.cons(new LispSymbol("a"), LispSymbol.ourNil));
        a = list.toLispObjectList();
        Assert.assertEquals(2, a.size());
        Assert.assertTrue(a.get(1) instanceof LispList);
    }

    @Test
    public void testToObjectListCons() {
        LispList list = LispList.cons(new LispSymbol("a"), LispList.cons(new LispSymbol("a"), LispSymbol.ourNil));
        List<LispObject> a = list.toLispObjectList();
        Assert.assertEquals("(a a)", list.toString());
        Assert.assertEquals(2, a.size());
        Assert.assertTrue(list.cdr() instanceof LispList);
    }

    @Test
    public void testAppend() {
        LispList a = LispList.list(new LispSymbol("a"));
        LispList b = LispList.list(new LispSymbol("b"));
        a.append(b);
        Assert.assertEquals(LispList.list(new LispSymbol("a"), new LispSymbol("b")), a);
    }

    @Test
    public void testAppendCons() {
        try {
            LispList a = LispList.cons(new LispSymbol("a"), new LispInteger(1));
            LispList b = LispList.list(new LispSymbol("b"));
            a.append(b);
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 1)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testLength() {
        LispList list = LispList.list();
        Assert.assertEquals(0, list.length());
        list = LispList.cons(LispSymbol.ourNil, null);
        Assert.assertEquals(1, list.length());
        list = LispList.list(new LispInteger(1), new LispInteger(1), new LispInteger(1), new LispInteger(1), new LispInteger(1));
        Assert.assertEquals(5, list.length());
        try {
            list = LispList.cons(new LispInteger(1), new LispInteger(2));
            list.length();
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument listp 2)", TestSetup.getCause(e));
            return;
        }
        Assert.fail();
    }
}