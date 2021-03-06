package org.jetbrains.emacs4ij.jelisp.elisp;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.JelispTestCase;
import org.jetbrains.emacs4ij.jelisp.subroutine.BList;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class LispListTest {
  @Test
  public void testCreateTrueList () {
    LispList list = LispList.list();
    Assert.assertEquals(LispSymbol.NIL, list);
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
    LispList list = LispList.cons(new LispInteger(1), LispSymbol.NIL);
    Assert.assertEquals("(1)", list.toString());
    list = LispList.cons(LispSymbol.NIL, new LispInteger(1));
    Assert.assertEquals("(nil . 1)", list.toString());
    list = LispList.cons(LispSymbol.NIL, LispSymbol.NIL);
    Assert.assertEquals("(nil)", list.toString());
  }

  @Test
  public void testToString() {
    LispList list = LispList.list(LispList.list(new LispInteger(1), new LispInteger(2)), LispSymbol.NIL);
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
    LispList list = LispList.list(LispSymbol.NIL, LispSymbol.NIL);
    Assert.assertEquals("(nil nil)", list.toString());
    ArrayList<LispObject> tList = new ArrayList<LispObject> ();
    tList.add(LispSymbol.NIL);
    tList.add(LispSymbol.NIL);
    Assert.assertEquals(tList, list.toLispObjectList());
  }

  @Test
  public void testtool() {
    LispList list = LispList.list(new LispInteger(1), new LispInteger(2));
    list.toString();
  }

  @Test
  public void testToObjectList() {
    LispList list = LispList.list(LispList.list(new LispSymbol("a")), LispList.cons(LispSymbol.NIL, LispList.list(new LispSymbol("b"))));
    List<LispObject> a = list.toLispObjectList();
    Assert.assertEquals(2, a.size());

    list = LispList.list(new LispInteger(1), LispList.cons(new LispInteger(2), new LispInteger(3)), new LispInteger(4));
    a = list.toLispObjectList();
    Assert.assertEquals(3, a.size());

    list = LispList.list(new LispSymbol("a"), LispList.list(new LispSymbol("b"), new LispSymbol("c")));
    a = list.toLispObjectList();
    Assert.assertEquals(2, a.size());
    Assert.assertTrue(a.get(1) instanceof LispList);

    list = LispList.list(new LispSymbol("a"), LispList.cons(new LispSymbol("a"), LispSymbol.NIL));
    a = list.toLispObjectList();
    Assert.assertEquals(2, a.size());
    Assert.assertTrue(a.get(1) instanceof LispList);
  }

  @Test
  public void testToObjectListCons() {
    LispList list = LispList.cons(new LispSymbol("a"), LispList.cons(new LispSymbol("a"), LispSymbol.NIL));
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
      Assert.assertEquals("'(wrong-type-argument listp 1)", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testSize() {
    LispList list = LispList.list();
    Assert.assertEquals(0, list.size());
    list = LispList.cons(LispSymbol.NIL, null);
    Assert.assertEquals(1, list.size());
    list = LispList.list(new LispInteger(1), new LispInteger(1), new LispInteger(1), new LispInteger(1), new LispInteger(1));
    Assert.assertEquals(5, list.size());
    try {
      list = LispList.cons(new LispInteger(1), new LispInteger(2));
      list.size();
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument listp 2)", JelispTestCase.getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  @Test
  public void testResetEmptyWithEmpty() {
    LispList current = LispList.list();
    LispList list = LispList.list();
    current.resetWith(list);
    Assert.assertEquals(LispList.list(), current);
  }

  @Test
  public void testResetEmptyWithOneElt() {
    LispList current = LispList.list();
    LispList list = LispList.list(new LispInteger(1));
    current.resetWith(list);
    Assert.assertEquals(LispList.list(new LispInteger(1)), current);
  }

  @Test
  public void testResetEmptyWithNilElt() {
    LispList current = LispList.list();
    LispList list = LispList.list(LispSymbol.NIL);
    current.resetWith(list);
    Assert.assertEquals(LispList.list(LispSymbol.NIL), current);
  }

  @Test
  public void testResetEmptyWith3Elt() {
    LispList current = LispList.list();
    LispList list = LispList.list(LispSymbol.NIL, new LispInteger(1), LispSymbol.NIL);
    current.resetWith(list);
    Assert.assertEquals(LispList.list(LispSymbol.NIL, new LispInteger(1), LispSymbol.NIL), current);
  }

  @Test
  public void testResetWithEqualLengthList() {
    LispList current = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
    LispList list = LispList.list(LispSymbol.NIL, new LispInteger(1), LispSymbol.NIL);
    current.resetWith(list);
    Assert.assertEquals(LispList.list(LispSymbol.NIL, new LispInteger(1), LispSymbol.NIL), current);
  }

  @Test
  public void testResetWithLessBy1List() {
    LispList current = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3));
    LispList list = LispList.list(LispSymbol.NIL, new LispInteger(1));
    current.resetWith(list);
    Assert.assertEquals(LispList.list(LispSymbol.NIL, new LispInteger(1), LispSymbol.NIL), current);
  }

  @Test
  public void testResetWithLessBy3List() {
    LispList current = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3), new LispInteger(4), new LispInteger(5));
    LispList list = LispList.list(LispSymbol.NIL, new LispInteger(1));
    current.resetWith(list);
    Assert.assertEquals(LispList.list(LispSymbol.NIL, new LispInteger(1), LispSymbol.NIL, LispSymbol.NIL, LispSymbol.NIL), current);
  }

  @Test
  public void testResetWithEmptyList() {
    LispList current = LispList.list(new LispInteger(1), new LispInteger(2), new LispInteger(3), new LispInteger(4), new LispInteger(5));
    LispList list = LispList.list();
    current.resetWith(list);
    Assert.assertEquals(LispList.list(LispSymbol.NIL, LispSymbol.NIL, LispSymbol.NIL, LispSymbol.NIL, LispSymbol.NIL), current);
  }
}