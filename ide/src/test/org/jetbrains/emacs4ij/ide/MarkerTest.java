package org.jetbrains.emacs4ij.ide;

import org.jetbrains.emacs4ij.jelisp.DefinitionLoader;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Before;

public class MarkerTest extends IdeTestCase {
  @Before
  public void setUp() throws Exception {
    super.setUp();
    setTestFiles(true);
    evaluateString("(switch-to-buffer \"3.txt\")");
    DefinitionLoader.loadEmacsFile("simple.el");
  }

  public void testSetMarker() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    LispObject marker = evaluateString("(setq m (point-marker))");
    Assert.assertEquals("#<marker at 1 in 3.txt>", marker.toString());
    marker = evaluateString("(set-marker m 5)");
    Assert.assertEquals("#<marker at 5 in 3.txt>", marker.toString());
    marker = evaluateString("(set-marker m 0 (get-buffer \"2.txt\"))");
    Assert.assertEquals("#<marker at 1 in 2.txt>", marker.toString());
    evaluateString("(setq m2 (copy-marker m))");
    marker = evaluateString("(set-marker m2 2 (get-buffer \"1.txt\"))");
    Assert.assertEquals("#<marker at 2 in 1.txt>", marker.toString());
    marker = evaluateString("(set-marker m m2)");
    Assert.assertEquals("#<marker at 2 in 3.txt>", marker.toString());
  }

  public void testMakeMarker() {
    LispObject m = evaluateString("(setq m1 (make-marker))");
    Assert.assertEquals("#<marker in no buffer>", m.toString());
  }

  public void testMarkerMoving() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    evaluateString("(set-mark 2)");
    evaluateString("(setq m1 (make-marker))");
    LispObject m = evaluateString("(set-marker m1 5)");
    Assert.assertEquals("#<marker at 5 in 3.txt>", m.toString());
    m = evaluateString("(goto-char (point-min))");
    Assert.assertEquals(new LispInteger(1), m);
    m = evaluateString("(insert \"Q\")");
    Assert.assertEquals(LispSymbol.NIL, m);
    m = evaluateString("m1");
    Assert.assertEquals("#<marker at 6 in 3.txt>", m.toString());
    m = evaluateString("(set-marker m1 nil)");
    Assert.assertEquals("#<marker in no buffer>", m.toString());
  }

  public void testMarkerEquality() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    evaluateString("(setq m1 (make-marker))");
    evaluateString("(set-marker m1 6)");
    LispObject m2 = evaluateString("(setq m2 (copy-marker m1))");
    Assert.assertEquals("#<marker at 6 in 3.txt>", m2.toString());
    m2 = evaluateString("(eq m1 m2)");
    Assert.assertEquals(LispSymbol.NIL, m2);
    m2 = evaluateString("(equal m1 m2)");
    Assert.assertEquals(LispSymbol.T, m2);
  }

  public void testGetMarkerPos () {
    evaluateString("(setq m (point-marker))");
    LispObject p = evaluateString("(marker-position m)");
    Assert.assertEquals(new LispInteger(1), p);
    evaluateString("(set-marker m nil)");
    p = evaluateString("(marker-position m)");
    Assert.assertEquals(LispSymbol.NIL, p);
  }

  public void testGetMarkerBuffer() {
    evaluateString("(setq m (point-marker))");
    LispObject b = evaluateString("(marker-buffer m)");
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), b);
  }

  public void testBufferHasMarkersAt() {
    evaluateString("(setq m (point-marker))");
    LispObject b = evaluateString("(buffer-has-markers-at 1)");
    Assert.assertEquals(LispSymbol.T, b);
    b = evaluateString("(buffer-has-markers-at nil)");
    Assert.assertEquals(LispSymbol.NIL, b);
    b = evaluateString("(buffer-has-markers-at 2)");
    Assert.assertEquals(LispSymbol.NIL, b);
    b = evaluateString("(buffer-has-markers-at \"hello\")");
    Assert.assertEquals(LispSymbol.NIL, b);
  }

//    //todo fix CoreTest.testSimple first
//    public void testMark() {
//        try {
//            DefinitionLoader.loadFile("simple.el");
//            evaluateString("(mark)");
//        } catch (Exception e) {
//            Assert.assertTrue(getCause(e).getMessage().contains("mark-inactive"));
//            return;
//        }
//        Assert.fail();
//    }

  public void testMarkForced() {
    LispObject mark = evaluateString("(mark t)");
    Assert.assertEquals(LispSymbol.NIL, mark);
  }

  public void testSetMark() throws Throwable {
    try {
      evaluateString("(switch-to-buffer \"3.txt\")");
      LispObject mark = evaluateString("(set-mark 5)");
      Assert.assertEquals("#<marker at 5 in 3.txt>", mark.toString());
      mark = evaluateString("(set-mark 50)");
      LispInteger pointMax = (LispInteger) evaluateString("(point-max)");
      Assert.assertEquals("#<marker at " + pointMax.getPosition() + " in 3.txt>", mark.toString());
    } catch (Exception e) {
      Throwable cause = getCause(e);
      System.out.println(cause.getMessage());
      throw cause;
    }
  }

  public void testMarkMarker() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    LispObject m = evaluateString("(setq m (mark-marker))");
    Assert.assertEquals("#<marker in no buffer>", m.toString());
    m = evaluateString("(set-marker m 5)");
    Assert.assertEquals("#<marker at 5 in 3.txt>", m.toString());
    m = evaluateString("(mark-marker)");
    Assert.assertEquals("#<marker at 5 in 3.txt>", m.toString());
  }

  public void testPushMark () {
    LispObject r = evaluateString("(push-mark)");
    Assert.assertEquals(LispSymbol.NIL, r);
    r = evaluateString("(mark t)");
    Assert.assertEquals(new LispInteger(1), r);

    r = evaluateString("(push-mark 5)");
    Assert.assertEquals(LispSymbol.NIL, r);

    r = evaluateString("(push-mark 5 t)");
    Assert.assertEquals(LispSymbol.NIL, r);

    r = evaluateString("(push-mark 5 t t)");
    Assert.assertEquals(LispSymbol.NIL, r);
    r = evaluateString("(mark t)");
    Assert.assertEquals(new LispInteger(5), r);
  }

  public void testPopMark () {
    evaluateString("(push-mark 5 t t)");
    evaluateString("(pop-mark)");
    LispObject r = evaluateString("(mark t)");
    Assert.assertEquals(new LispInteger(5), r);
  }

  //TODO: test function handle-shift-selection

  public void testRegionBeginning() {
    LispObject r = evaluateString("(region-beginning)");
    Assert.assertEquals(new LispInteger(1), r);
    evaluateString("(set-mark 2)");
    myEnvironment.getBufferCurrentForEditing().gotoChar(6);
    r = evaluateString("(region-beginning)");
    Assert.assertEquals(new LispInteger(2), r);
  }

  public void testRegionEnd() {
    LispObject r = evaluateString("(region-end)");
    Assert.assertEquals(new LispInteger(1), r);
    evaluateString("(set-mark 5)");
    r = evaluateString("(region-end)");
    Assert.assertEquals(new LispInteger(5), r);
  }

  public void testUseRegionP() {
    LispObject r = evaluateString("(use-region-p)");
    Assert.assertEquals(LispSymbol.NIL, r);
  }

  public void testInsertNil() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 2)");
    System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
    evaluateString("(prin1 1 m)");
    LispMarker m = (LispMarker) evaluateString("m");
    System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
    Assert.assertEquals("3", m.getPosition().toString());
  }

  public void testInsertT() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    evaluateString("(set-marker-insertion-type m t)");
    System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
    evaluateString("(prin1 1 m)");
    LispMarker m = (LispMarker) evaluateString("m");
    System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
    Assert.assertEquals("4", m.getPosition().toString());
  }

  public void testInsertKey() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    evaluateString("(set-mark 2)");
    System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
    evaluateString("(prin1 \"hello\" m)");
    LispMarker m = (LispMarker) evaluateString("m");
    System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
    Assert.assertEquals("10", m.getPosition().toString());
  }

  public void testKbdEscQuit() {
    evaluateString("(keyboard-escape-quit)");
  }

  public void testMatchDataReuseListWithMarker() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    LispMarker m = (LispMarker) evaluateString("m");
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

    evaluateString("(setq reuse (list 1 2 m))");
    LispObject data = evaluateString("reuse");
    Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(2), m), data);

    evaluateString("(string-match \"quick\" \"The quick fox jumped quickly.\")");
    data = evaluateString("(match-data nil reuse)");
    Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.NIL), data);
    data = evaluateString("reuse");
    Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.NIL), data);
    data = evaluateString("m");
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), data);

    evaluateString("(setq reuse (list 1 2 m))");
    data = evaluateString("(match-data nil reuse t)");
    Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.NIL), data);
    data = evaluateString("reuse");
    Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.NIL), data);
    data = evaluateString("m");
    Assert.assertFalse(m.isSet());
    Assert.assertEquals(m, data);
  }

  public void testSetMatchDataInteger() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    LispMarker m = (LispMarker) evaluateString("m");
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

    evaluateString("(setq m2 (make-marker))");
    evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
    LispMarker m2 = (LispMarker) evaluateString("m2");
    Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

    evaluateString("(set-match-data (list m 1 m2))");
    LispObject data = evaluateString("(match-data)");
    LispMarker marker = new LispMarker(1, myEnvironment.getBufferCurrentForEditing());
    Assert.assertEquals(LispList.list(m, marker), data);
    data = evaluateString("(match-data t)");
    Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(1), myEnvironment.getBufferCurrentForEditing()), data);

    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(m, marker), data);
  }

  public void testSetMatchDataNowhereMarker() {
    LispObject data = evaluateString("(set-match-data (list (make-marker) 1 2))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(1)), data);
  }

  public void testSetMatchDataMarkerFirst() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    LispMarker m = (LispMarker) evaluateString("m");
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

    LispObject data = evaluateString("(set-match-data (list m 1))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    LispMarker m2 = new LispMarker(1, myEnvironment.getBufferCurrentForEditing());
    Assert.assertEquals(LispList.list(m, m2), data);
  }

  public void testSetMatchDataMarkerSecond() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    LispMarker m = (LispMarker) evaluateString("m");
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

    LispObject data = evaluateString("(set-match-data (list 1 m))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(3)), data);
  }

  public void testSetMatchDataB1B2() {
    evaluateString("(setq m1 (make-marker))");
    evaluateString("(set-marker m1 3 (get-buffer \"1.txt\"))");
    LispMarker m1 = (LispMarker) evaluateString("m1");
    Assert.assertEquals(new LispMarker(3, myTests.get("1.txt")), m1);

    evaluateString("(setq m2 (make-marker))");
    evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
    LispMarker m2 = (LispMarker) evaluateString("m2");
    Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

    LispObject data = evaluateString("(set-match-data (list m1 1 m2 2))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
        m2, new LispMarker(2, myTests.get("2.txt"))), data);
  }

  public void testSetMatchDataB1() {
    evaluateString("(setq m2 (make-marker))");
    evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
    LispMarker m2 = (LispMarker) evaluateString("m2");
    Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

    LispObject data = evaluateString("(set-match-data (list 3 1 m2 2))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
        m2, new LispMarker(2, myTests.get("2.txt"))), data);
  }

  public void testSetMatchDataN() {
    evaluateString("(setq m2 (make-marker))");
    evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
    LispMarker m2 = (LispMarker) evaluateString("m2");
    Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

    LispObject data = evaluateString("(set-match-data (list 3 1 2 m2))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(1), new LispInteger(2), new LispInteger(4)), data);
  }

  public void testSetMatchDataB1B1B2() {
    evaluateString("(setq m1 (make-marker))");
    evaluateString("(set-marker m1 3 (get-buffer \"1.txt\"))");
    LispMarker m1 = (LispMarker) evaluateString("m1");
    Assert.assertEquals(new LispMarker(3, myTests.get("1.txt")), m1);

    evaluateString("(setq m2 (make-marker))");
    evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
    LispMarker m2 = (LispMarker) evaluateString("m2");
    Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

    LispObject data = evaluateString("(set-match-data (list m1 1 m1 5 m2 2))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
        new LispMarker(3, myTests.get("2.txt")), new LispMarker(5, myTests.get("2.txt")),
        m2, new LispMarker(2, myTests.get("2.txt"))), data);
  }

  public void testSetMatchDataB1B1E() {
    evaluateString("(setq m1 (make-marker))");
    evaluateString("(set-marker m1 3 (get-buffer \"1.txt\"))");
    LispMarker m1 = (LispMarker) evaluateString("m1");
    Assert.assertEquals(new LispMarker(3, myTests.get("1.txt")), m1);

    evaluateString("(setq m2 (make-marker))");
    evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
    LispMarker m2 = (LispMarker) evaluateString("m2");
    Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

    evaluateString("(setq e (make-marker))");

    LispObject data = evaluateString("(set-match-data (list m1 1 m2 5 e 2))");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
        m2, new LispMarker(5, myTests.get("2.txt")),
        new LispMarker(1, myTests.get("2.txt")), new LispMarker(2, myTests.get("2.txt"))), data);
  }

  public void testSetMatchDataResetMarker() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 3)");
    LispMarker m = (LispMarker) evaluateString("m");
    Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

    LispObject data = evaluateString("(set-match-data (list m 1) t)");
    Assert.assertEquals(LispSymbol.NIL, data);
    data = evaluateString("(match-data)");
    Assert.assertEquals(LispList.list(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()),
        new LispMarker(1, myEnvironment.getBufferCurrentForEditing())), data);
    Assert.assertFalse(m.isSet());
  }

  public void testMatchStringInBuffer() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    evaluateString("(set-match-data (list 1 2))");
    LispObject match = evaluateString("(match-string 0)");
    Assert.assertEquals(new LispString("l"), match);
  }

  public void testReplaceMatchInStringShorter () {
    evaluateString("(set-match-data (list 1 2))");
    String init = myEnvironment.getBufferCurrentForEditing().getText();
    LispObject replaced = evaluateString("(replace-match \"anna\")");
    Assert.assertEquals(LispSymbol.NIL, replaced);
    String expected ="anna" + init.substring(1);
    Assert.assertEquals(expected, myEnvironment.getBufferCurrentForEditing().getText());
    Assert.assertEquals(5, myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testEql() {
    evaluateString("(setq m (make-marker))");
    evaluateString("(set-marker m 1)");
    LispObject eq = evaluateString("(eql m 1)");
    Assert.assertEquals(LispSymbol.NIL, eq);
  }

  public void testReplaceMatchInBuffer () {
    evaluateString("(switch-to-buffer \"1.txt\")");
    evaluateString("(set-match-data '(2 10 1 3 4 6))");
    String init = myEnvironment.getBufferCurrentForEditing().getText();
    evaluateString("(replace-match \"one\\2two\")");
    String expected = init.substring(0, 1) + "one" + init.substring(3, 5) + "two" + init.substring(9);
    Assert.assertEquals(expected, myEnvironment.getBufferCurrentForEditing().getText());
    Assert.assertEquals(10, myEnvironment.getBufferCurrentForEditing().point());
  }
}
