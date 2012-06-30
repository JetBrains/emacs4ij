package org.jetbrains.emacs4ij;

import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/21/11
 * Time: 8:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class MarkerTest extends CodeInsightFixtureTestCase {
    CustomEnvironment myEnvironment;
    ForwardParser myForwardParser = new ForwardParser();
    String myTestsPath;
    HashMap<String, IdeaBuffer> myTests = new HashMap<>();
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        myTestsPath = TestSetup.setGlobalEnv();
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        List<String> list = Arrays.asList((new File(myTestsPath)).list());
        Collections.sort(list);
        for (String fileName: list) {
            PsiFile psiFile = myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, psiFile.getVirtualFile(), getEditor());
            myTests.put(fileName, buffer);
        }
        Collections.reverse(list);
        myTestFiles = list.toArray(new String[list.size()]);
        evaluateString("(switch-to-buffer \"3.txt\")");
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    private LispObject evaluateString (String lispCode) {
        return myForwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
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

    @Test
    public void testMakeMarker() {
        LispObject m = evaluateString("(setq m1 (make-marker))");
        Assert.assertEquals("#<marker in no buffer>", m.toString());
    }

    @Test
    public void testMarkerMoving() {
        evaluateString("(switch-to-buffer \"3.txt\")");
        evaluateString("(setq m1 (make-marker))");
        LispObject m = evaluateString("(set-marker m1 5)");
        Assert.assertEquals("#<marker at 5 in 3.txt>", m.toString());
        m = evaluateString("(goto-char (point-min))");
        Assert.assertEquals(new LispInteger(1), m);
        m = evaluateString("(insert \"Q\")");
        Assert.assertEquals(LispSymbol.ourNil, m);
        m = evaluateString("m1");
        Assert.assertEquals("#<marker at 6 in 3.txt>", m.toString());
        m = evaluateString("(set-marker m1 nil)");
        Assert.assertEquals("#<marker in no buffer>", m.toString());
    }

    @Test
    public void testMarkerEquality() {
        evaluateString("(switch-to-buffer \"3.txt\")");
        evaluateString("(setq m1 (make-marker))");
        evaluateString("(set-marker m1 6)");
        LispObject m2 = evaluateString("(setq m2 (copy-marker m1))");
        Assert.assertEquals("#<marker at 6 in 3.txt>", m2.toString());
        m2 = evaluateString("(eq m1 m2)");
        Assert.assertEquals(LispSymbol.ourNil, m2);
        m2 = evaluateString("(equal m1 m2)");
        Assert.assertEquals(LispSymbol.ourT, m2);
    }

    @Test
    public void testGetMarkerPos () {
        evaluateString("(setq m (point-marker))");
        LispObject p = evaluateString("(marker-position m)");
        Assert.assertEquals(new LispInteger(1), p);
        evaluateString("(set-marker m nil)");
        p = evaluateString("(marker-position m)");
        Assert.assertEquals(LispSymbol.ourNil, p);
    }

    @Test
    public void testGetMarkerBuffer() {
        evaluateString("(setq m (point-marker))");
        LispObject b = evaluateString("(marker-buffer m)");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), b);
    }

    @Test
    public void testBufferHasMarkersAt() {
        evaluateString("(setq m (point-marker))");
        LispObject b = evaluateString("(buffer-has-markers-at 1)");
        Assert.assertEquals(LispSymbol.ourT, b);
        b = evaluateString("(buffer-has-markers-at nil)");
        Assert.assertEquals(LispSymbol.ourNil, b);
        b = evaluateString("(buffer-has-markers-at 2)");
        Assert.assertEquals(LispSymbol.ourNil, b);
        b = evaluateString("(buffer-has-markers-at \"hello\")");
        Assert.assertEquals(LispSymbol.ourNil, b);
    }


    @Test
    public void testMark () throws Throwable {
        try {
            evaluateString("(mark)");
        } catch (Exception e) {
            Assert.assertTrue(getCause(e).getMessage().contains("mark-inactive"));
            return;
        }
        Assert.fail();
    }

    @Test
    public void testMarkForced () throws Throwable {
        LispObject mark = evaluateString("(mark t)");
        Assert.assertEquals(LispSymbol.ourNil, mark);
    }

    @Test
    public void testSetMark () throws Throwable {
        try {
            evaluateString("(switch-to-buffer \"3.txt\")");
            LispObject mark = evaluateString("(set-mark 5)");
            Assert.assertEquals("#<marker at 5 in 3.txt>", mark.toString());
            mark = evaluateString("(set-mark 50)");
            LispInteger pointMax = (LispInteger) evaluateString("(point-max)");
            Assert.assertEquals("#<marker at " + pointMax.getPosition() + " in 3.txt>", mark.toString());
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
            throw getCause(e);
        }
    }

    @Test
    public void testMarkMarker() {
        evaluateString("(switch-to-buffer \"3.txt\")");
        LispObject m = evaluateString("(setq m (mark-marker))");
        Assert.assertEquals("#<marker in no buffer>", m.toString());
        m = evaluateString("(set-marker m 5)");
        Assert.assertEquals("#<marker at 5 in 3.txt>", m.toString());
        m = evaluateString("(mark-marker)");
        Assert.assertEquals("#<marker at 5 in 3.txt>", m.toString());
    }

    @Test
    public void testPushMark () {
        LispObject r = evaluateString("(push-mark)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(mark t)");
        Assert.assertEquals(new LispInteger(1), r);

        r = evaluateString("(push-mark 5)");
        Assert.assertEquals(LispSymbol.ourNil, r);

        r = evaluateString("(push-mark 5 t)");
        Assert.assertEquals(LispSymbol.ourNil, r);

        r = evaluateString("(push-mark 5 t t)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(mark t)");
        Assert.assertEquals(new LispInteger(5), r);
    }

    @Test
    public void testPopMark () {
        evaluateString("(push-mark 5 t t)");
        evaluateString("(pop-mark)");
        LispObject r = evaluateString("(mark t)");
        Assert.assertEquals(new LispInteger(5), r);
    }

    //TODO: test function handle-shift-selection

    @Test
    public void testRegionBeginning() {
        LispObject r = evaluateString("(region-beginning)");
        Assert.assertEquals(new LispInteger(1), r);
        evaluateString("(set-mark 2)");
        myEnvironment.getBufferCurrentForEditing().gotoChar(6);
        r = evaluateString("(region-beginning)");
        Assert.assertEquals(new LispInteger(2), r);
    }

    @Test
    public void testRegionEnd() {
        LispObject r = evaluateString("(region-end)");
        Assert.assertEquals(new LispInteger(1), r);
        evaluateString("(set-mark 5)");
        r = evaluateString("(region-end)");
        Assert.assertEquals(new LispInteger(5), r);
    }

    @Test
    public void testUseRegionP() {
        LispObject r = evaluateString("(use-region-p)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }
    
    @Test
    public void testInsertNil() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 2)");
        System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
        evaluateString("(prin1 1 m)");
        LispMarker m = (LispMarker) evaluateString("m");
        System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
        Assert.assertEquals("3", m.getPosition().toString());
    }

    @Test
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

    @Test
    public void testInsertKey() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 3)");
        System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
        evaluateString("(prin1 \"hello\" m)");
        LispMarker m = (LispMarker) evaluateString("m");
        System.out.println(myEnvironment.getBufferCurrentForEditing().getText());
        Assert.assertEquals("10", m.getPosition().toString());
    }

    @Test
    public void testKbdEscQuit() {
        evaluateString("(keyboard-escape-quit)");
    }

    @Test
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
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.ourNil), data);
        data = evaluateString("reuse");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.ourNil), data);
        data = evaluateString("m");
        Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);
        Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), data);

        evaluateString("(setq reuse (list 1 2 m))");
        data = evaluateString("(match-data nil reuse t)");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.ourNil), data);
        data = evaluateString("reuse");
        Assert.assertEquals(LispList.list(new LispInteger(4), new LispInteger(9), LispSymbol.ourNil), data);
        data = evaluateString("m");
        Assert.assertFalse(m.isSet());
        Assert.assertEquals(m, data);
    }

    @Test
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

    @Test
    public void testSetMatchDataNowhereMarker() {
        LispObject data = evaluateString("(set-match-data (list (make-marker) 1 2))");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(0), new LispInteger(1)), data);
    }

    @Test
    public void testSetMatchDataMarkerFirst() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 3)");
        LispMarker m = (LispMarker) evaluateString("m");
        Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

        LispObject data = evaluateString("(set-match-data (list m 1))");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        LispMarker m2 = new LispMarker(1, myEnvironment.getBufferCurrentForEditing());
        Assert.assertEquals(LispList.list(m, m2), data);

    }

    @Test
    public void testSetMatchDataMarkerSecond() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 3)");
        LispMarker m = (LispMarker) evaluateString("m");
        Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

        LispObject data = evaluateString("(set-match-data (list 1 m))");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(1), new LispInteger(3)), data);
    }

    @Test
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
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
                 m2, new LispMarker(2, myTests.get("2.txt"))), data);
    }

    @Test
    public void testSetMatchDataB1() {
        evaluateString("(setq m2 (make-marker))");
        evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
        LispMarker m2 = (LispMarker) evaluateString("m2");
        Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

        LispObject data = evaluateString("(set-match-data (list 3 1 m2 2))");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
                m2, new LispMarker(2, myTests.get("2.txt"))), data);
    }

    @Test
    public void testSetMatchDataN() {
        evaluateString("(setq m2 (make-marker))");
        evaluateString("(set-marker m2 4 (get-buffer \"2.txt\"))");
        LispMarker m2 = (LispMarker) evaluateString("m2");
        Assert.assertEquals(new LispMarker(4, myTests.get("2.txt")), m2);

        LispObject data = evaluateString("(set-match-data (list 3 1 2 m2))");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispInteger(3), new LispInteger(1), new LispInteger(2), new LispInteger(4)), data);
    }

    @Test
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
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
                new LispMarker(3, myTests.get("2.txt")), new LispMarker(5, myTests.get("2.txt")),
                m2, new LispMarker(2, myTests.get("2.txt"))), data);
    }

    @Test
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
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispMarker(3, myTests.get("2.txt")), new LispMarker(1, myTests.get("2.txt")),
                m2, new LispMarker(5, myTests.get("2.txt")),
                new LispMarker(1, myTests.get("2.txt")), new LispMarker(2, myTests.get("2.txt"))), data);
    }

    @Test
    public void testSetMatchDataResetMarker() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 3)");
        LispMarker m = (LispMarker) evaluateString("m");
        Assert.assertEquals(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()), m);

        LispObject data = evaluateString("(set-match-data (list m 1) t)");
        Assert.assertEquals(LispSymbol.ourNil, data);
        data = evaluateString("(match-data)");
        Assert.assertEquals(LispList.list(new LispMarker(3, myEnvironment.getBufferCurrentForEditing()),
                new LispMarker(1, myEnvironment.getBufferCurrentForEditing())), data);
        Assert.assertFalse(m.isSet());
    }

    @Test
    public void testMatchStringInBuffer() {
        evaluateString("(switch-to-buffer \"3.txt\")");
        evaluateString("(set-match-data (list 1 2))");
        LispObject match = evaluateString("(match-string 0)");
        Assert.assertEquals(new LispString("l"), match);
    }

    @Test
    public void testReplaceMatchInStringShorter () {
        evaluateString("(set-match-data (list 1 2))");
        String init = myEnvironment.getBufferCurrentForEditing().getText();
        LispObject replaced = evaluateString("(replace-match \"anna\")");
        Assert.assertEquals(LispSymbol.ourNil, replaced);
        String expected ="anna" + init.substring(1);
        Assert.assertEquals(expected, myEnvironment.getBufferCurrentForEditing().getText());
        Assert.assertEquals(5, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testEql() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 1)");
        LispObject eq = evaluateString("(eql m 1)");
        Assert.assertEquals(LispSymbol.ourNil, eq);
    }

    @Test
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
