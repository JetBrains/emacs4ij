package org.jetbrains.emacs4ij;

import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.HashMap;

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
    HashMap<String, IdeaBuffer> myTests;
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        myTestFiles = (new File(myTestsPath)).list();
        myTests = new HashMap<>();
        GlobalEnvironment.initialize(new KeymapCreator(), new IdeProvider(), new TestFrameManagerImpl());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        // GlobalEnvironment.getInstance().clearRecorded();
        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            myTests.put(fileName, new IdeaBuffer(myEnvironment, fileName, myTestsPath, getEditor()));
        }
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
            //GlobalEnvironment.INSTANCE.findAndRegisterEmacsFunction(GlobalEnvironment.ourEmacsSource + "/lisp/simple.el", "set-mark");
            LispObject mark = evaluateString("(set-mark 5)");
            Assert.assertEquals("#<marker at 5 in 3.txt>", mark.toString());
            mark = evaluateString("(set-mark 50)");
            Assert.assertEquals("#<marker at 9 in 3.txt>", mark.toString());
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
            throw getCause(e);
        }
    }

    @Test
    public void testMarkMarker() {
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
        evaluateString("(set-marker m 3)");
        evaluateString("(prin1 1 m)");
        LispMarker m = (LispMarker) evaluateString("m");
        Assert.assertEquals("4", m.getPosition().toString());

        System.out.println(myEnvironment.getBufferCurrentForEditing().getEditor().getDocument().getText());
    }

    @Test
    public void testInsertT() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 3)");
        evaluateString("(set-marker-insertion-type m t)");
        evaluateString("(prin1 1 m)");
        LispMarker m = (LispMarker) evaluateString("m");
        Assert.assertEquals("4", m.getPosition().toString());

        System.out.println(myEnvironment.getBufferCurrentForEditing().getEditor().getDocument().getText());
    }

    @Test
    public void testInsertKey() {
        evaluateString("(setq m (make-marker))");
        evaluateString("(set-marker m 3)");
        evaluateString("(prin1 \"hello\" m)");
        LispMarker m = (LispMarker) evaluateString("m");
        Assert.assertEquals("10", m.getPosition().toString());
        System.out.println(myEnvironment.getBufferCurrentForEditing().getEditor().getDocument().getText());
    }
}
