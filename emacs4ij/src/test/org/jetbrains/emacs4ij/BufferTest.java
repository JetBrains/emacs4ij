package org.jetbrains.emacs4ij;

import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.ForwardParser;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/22/11
 * Time: 3:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferTest extends CodeInsightFixtureTestCase {
    CustomEnvironment myEnvironment;
    ForwardParser myForwardParser = new ForwardParser();
    String myTestsPath;
    HashMap<String, IdeaBuffer> myTests = new HashMap<>();
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        myTestsPath = TestSetup.setGlobalEnv();
        super.setUp();
        myTestFiles = (new File(myTestsPath)).list();
        GlobalEnvironment.initialize(new BufferCreator(), new IdeProvider());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, fileName, myTestsPath, getEditor());
            myTests.put(fileName, buffer);
        }
    }

    private LispObject evaluateString(String lispCode) {
        return myForwardParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    @Test
    public void testCurrentBuffer() {
        LispObject lispObject = evaluateString("(current-buffer)");
        Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), lispObject);
    }

    @Test
    public void testBufferp() {
        LispObject lispObject = evaluateString("(bufferp (current-buffer))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testBufferpWrongNargs() {
        try {
            evaluateString("(bufferp)");
        } catch (WrongNumberOfArgumentsException e) {
            //success
        }
    }

    @Test
    public void testGetBufferByName() {
        LispObject lispObject = evaluateString("(get-buffer \"1.txt\")");
        Assert.assertEquals(myTests.get("1.txt"), lispObject);
    }

    @Test
    public void testGetBufferByBuffer() {
        LispObject lispObject = evaluateString("(get-buffer (current-buffer))");
        Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), lispObject);
    }

    @Test
    public void testGetBuffer_NonExistent() {
        LispObject lispObject = evaluateString("(get-buffer \"test.txt\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testGetBuffer_Nil() {
        try {
            evaluateString("(get-buffer nil)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
        }
    }

    @Test
    public void testSetBufferWrongType() {
        try {
            evaluateString("(set-buffer 5)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
        }
    }

    @Test
    public void testBufferSize() {
        LispObject currentBufferSizeAnonymous = evaluateString("(buffer-size)");
        LispObject currentBufferSize = evaluateString("(buffer-size (current-buffer))");
        Assert.assertEquals(currentBufferSize, currentBufferSizeAnonymous);
    }

    @Test
    public void testBufferSizeNil() {
        LispObject currentBufferSizeAnonymous = evaluateString("(buffer-size nil)");
        LispObject currentBufferSize = evaluateString("(buffer-size (current-buffer))");
        Assert.assertEquals(currentBufferSize, currentBufferSizeAnonymous);
    }

    @Test
    public void testBufferName () {
        LispObject lispObject = evaluateString("(buffer-name (get-buffer \"1.txt\"))");
        Assert.assertEquals(new LispString("1.txt"), lispObject);
    }

    @Test
    public void testBufferName_Nil () {
        LispObject currentBufferName = evaluateString("(buffer-name (current-buffer))");
        LispObject lispObject = evaluateString("(buffer-name)");
        Assert.assertEquals(currentBufferName, lispObject);
        lispObject = evaluateString("(buffer-name nil)");
        Assert.assertEquals(currentBufferName, lispObject);
    }

    @Test
    public void testOtherBuffer_SingleBuffer () {
        while (myEnvironment.getBuffersSize() != 1)
            myEnvironment.closeCurrentBuffer();
        LispObject lispObject = evaluateString("(other-buffer)");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
    }

    @Test
    public void testOtherBuffer_NoBuffers () {
        myEnvironment.closeAllBuffers();
        try {
            evaluateString("(other-buffer)");
        } catch (RuntimeException e) {
            Assert.assertEquals(new NoOpenedBufferException().getMessage(), getCause(e).getMessage());
            return;
        }
        Assert.assertEquals(1, 0);
    }

    @Test
    public void testOtherBuffer_NoParameters3buffers () {
        LispObject lispObject = evaluateString("(other-buffer)");
        Assert.assertEquals(myEnvironment.getBufferByIndex(myEnvironment.getBuffersSize()-2), lispObject);
    }

    @Test
    public void testOtherBuffer_NotBufferParameter () {
        LispObject lispObject = evaluateString("(other-buffer 1)");
        Assert.assertEquals(myEnvironment.getBufferByIndex(myEnvironment.getBuffersSize()-2), lispObject);
    }

    @Test
    public void testOtherBuffer_BufferParameter () {
        LispObject lispObject = evaluateString("(other-buffer (get-buffer \"" + myTestFiles[0] + "\" ))");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
    }

    @Test
    public void testEnvironment_GetBuffersNames() {
        String[] buffersNames = myEnvironment.getBuffersNames();
        Assert.assertArrayEquals(myTestFiles, buffersNames);
    }

    @Test
    public void testSwitchToBuffer_NoArgs() {
        try {
            evaluateString("(switch-to-buffer)");
        } catch (WrongNumberOfArgumentsException e) {
            //success
            return;
        }
        Assert.assertEquals(0, 1);
    }

    @Test
    public void testSwitchToBuffer_Nil() {
        LispObject lispObject = evaluateString("(switch-to-buffer nil)");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_ExistentString() {
        LispObject lispObject = evaluateString("(switch-to-buffer \"" + myTestFiles[0] + "\")");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_NonExistentString() {
        LispObject lispObject = evaluateString("(switch-to-buffer \"test.txt\")");
        Assert.assertEquals(new LispString("It is not allowed to create files this way."), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_Buffer() {
        LispObject lispObject = evaluateString("(switch-to-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_ExistentString_NoRecord() {
        LispObject lispObject = evaluateString("(switch-to-buffer \"" + myTestFiles[0] + "\" 5)");
        Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferByIndex(0).getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSetBuffer_String () {
        LispObject lispObject = evaluateString("(set-buffer \"" + myTestFiles[0] + "\")");
        Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
    }

    @Test
    public void testSetBuffer_Buffer () {
        LispObject lispObject = evaluateString("(set-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
        Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
    }

    @Test
    public void testSetBuffer_InProgn () {
        LispObject buffer = evaluateString("(progn (set-buffer \"" + myTestFiles[0] + "\") (buffer-name))");
        Assert.assertEquals(new LispString(myTestFiles[0]), buffer);
        evaluateString("(set-buffer \"" + myTestFiles[0] + "\")");
        buffer = evaluateString("(buffer-name)");
        Assert.assertEquals(new LispString(myTestFiles[myTestFiles.length-1]), buffer);
    }

    @Test
    public void testSetBuffer_WrongParameter () {
        try {
            evaluateString("(set-buffer 5)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
        }
    }

    @Test
    public void testPoint() {
        LispObject lispObject = evaluateString("(point)");
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        int point = fileEditorManager.getSelectedTextEditor().logicalPositionToOffset(fileEditorManager.getSelectedTextEditor().getCaretModel().getLogicalPosition()) + 1;
        Assert.assertEquals(new LispInteger(point), lispObject);
    }

    @Test
    public void testPointMin() {
        LispObject lispObject = evaluateString("(point-min)");
        Assert.assertEquals(new LispInteger(1), lispObject);
    }

    @Test
    public void testPointMax() {
        LispObject lispObject = evaluateString("(point-max)");
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        int pointMax = fileEditorManager.getSelectedTextEditor().getDocument().getTextLength()+1;
        Assert.assertEquals(new LispInteger(pointMax), lispObject);
    }

    @Test
    public void testGoToChar () {
        LispObject lispObject = evaluateString("(goto-char 5)");
        Assert.assertEquals(new LispInteger(5), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().point(), 5);
    }

    @Test
    public void testGoToChar_End () {
        LispObject lispObject = evaluateString("(goto-char 200)");
        Assert.assertEquals(new LispInteger(200), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testGoToChar_Begin () {
        LispObject lispObject = evaluateString("(goto-char -50)");
        Assert.assertEquals(new LispInteger(-50), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar_NoParam () {
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LispObject lispObject = evaluateString("(forward-char)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from+1, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar () {
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LispObject lispObject = evaluateString("(forward-char 5)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from+5, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar_End () {
        LispObject lispObject = evaluateString("(forward-char 200)");
        Assert.assertEquals(new LispSymbol("End of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar_Begin () {
        LispObject lispObject = evaluateString("(forward-char -50)");
        Assert.assertEquals(new LispSymbol("Beginning of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar_NoParam () {
        myEnvironment.getBufferCurrentForEditing().forwardChar(2);
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LispObject lispObject = evaluateString("(backward-char)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from-1, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar () {
        myEnvironment.getBufferCurrentForEditing().forwardChar(3);
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LispObject lispObject = evaluateString("(backward-char 2)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from-2, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar_End () {
        LispObject lispObject = evaluateString("(backward-char 50)");
        Assert.assertEquals(new LispSymbol("Beginning of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar_Begin () {
        LispObject lispObject = evaluateString("(backward-char -500)");
        Assert.assertEquals(new LispSymbol("End of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
    }

    //todo:: test mySymbols.put("buffer-end", new LispSymbol("buffer-end", LispSymbol.FunctionType.BuiltIn)); //note: it is compiled lisp function in emacs

    //==== markers ====

    @Test
    public void testPointMarkerOk() {
        LispObject lispObject = evaluateString("(point-marker)");
        LispBuffer currentBuffer = (LispBuffer) evaluateString("(current-buffer)");
        LispMarker marker = new LispMarker(currentBuffer.point(), currentBuffer);
        Assert.assertEquals(marker, lispObject);
    }

    @Test
    public void testPointMarkerInvalid () {
        try {
            evaluateString("(point-marker)");
        } catch (WrongNumberOfArgumentsException e) {
            //success
        }
    }

    @Test
    public void testPointMinMarker () {
        LispObject lispObject = evaluateString("(point-min-marker)");
        LispBuffer currentBuffer = (LispBuffer) evaluateString("(current-buffer)");
        Assert.assertEquals(new LispMarker(currentBuffer.pointMin(), currentBuffer), lispObject);
    }

    @Test
    public void testPointMaxMarker () {
        LispObject lispObject = evaluateString("(point-max-marker)");
        LispBuffer currentBuffer = (LispBuffer) evaluateString("(current-buffer)");
        Assert.assertEquals(new LispMarker(currentBuffer.pointMax(), currentBuffer), lispObject);
    }

    @Test
    public void testCopyMarker_Marker () {
        evaluateString("(defvar m1 (make-marker))");
        LispObject lispObject = evaluateString("(copy-marker m1)");
        LispMarker m1 = new LispMarker();
        Assert.assertEquals(m1, lispObject);

        lispObject = evaluateString("(copy-marker m1 nil)");
        Assert.assertEquals(m1, lispObject);

        lispObject = evaluateString("(copy-marker 50000 nil)");
        LispBuffer buffer = myEnvironment.getBufferCurrentForEditing();
        m1 = new LispMarker(buffer.pointMax(), buffer);
        Assert.assertEquals(m1, lispObject);

        m1.setPosition(buffer.pointMin());
        m1.setInsertionType(LispSymbol.ourT);
        lispObject = evaluateString("(copy-marker -50 t)");
        Assert.assertEquals(m1, lispObject);
    }

    @Test
    public void testEq() {
        LispObject lispObject = evaluateString("(eq (point-marker) (point-marker))");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(equal (point-marker) (point-marker))");
        junit.framework.Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testDefaultDirectory () {
        LispObject lispObject = evaluateString("default-directory");
        Assert.assertEquals(new LispString(myTestsPath), lispObject);
    }

    @Test
    public void testBufferList () {
        LispObject lispObject = evaluateString("(buffer-list)");
        Assert.assertEquals(myEnvironment.getBufferList(), lispObject);
    }

    @Test
    public void testBuryBuffer_Current () {
        LispBuffer current = myEnvironment.getBufferCurrentForEditing();
        ArrayList<LispBuffer> buffers = myEnvironment.getBuffers();
        buffers.remove(current);
        buffers.add(0, current);
        evaluateString("(bury-buffer)");
        Assert.assertEquals(buffers, myEnvironment.getBuffers());
    }

    @Test
    public void testBuryBuffer_String () {
        LispBuffer current = myEnvironment.findBufferSafe(myTestFiles[1]);
        ArrayList<LispBuffer> buffers = myEnvironment.getBuffers();
        buffers.remove(current);
        buffers.add(0, current);
        evaluateString("(bury-buffer \"" + myTestFiles[1] + "\")");
        Assert.assertEquals(buffers, myEnvironment.getBuffers());
    }

    @Test
    public void testBuryBuffer_Buffer () {
        LispBuffer current = myEnvironment.findBufferSafe(myTestFiles[1]);
        ArrayList<LispBuffer> buffers = myEnvironment.getBuffers();
        buffers.remove(current);
        buffers.add(0, current);
        evaluateString("(bury-buffer (get-buffer \"" + myTestFiles[1] + "\"))");
        Assert.assertEquals(buffers, myEnvironment.getBuffers());
    }

    @Test
    public void testLastBuffer () {
        try {
            LispObject lastBuffer = evaluateString("(last-buffer)");
            Assert.assertTrue(lastBuffer instanceof LispBuffer);
            Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
        }
    }

    @Test
    public void testLastBuffer_Integer () {
        LispObject lastBuffer = evaluateString("(last-buffer 1)");
        Assert.assertTrue(lastBuffer instanceof LispBuffer);
        Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
    }

    @Test
    public void testLastBuffer_Other () {
        LispObject lastBuffer = evaluateString("(last-buffer (get-buffer \"" + myTestFiles[myTestFiles.length - 1] + "\"))");
        Assert.assertTrue(lastBuffer instanceof LispBuffer);
        Assert.assertEquals(myTestFiles[myTestFiles.length-2], ((LispBuffer) lastBuffer).getName());
        lastBuffer = evaluateString("(last-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
        Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
    }

    @Test
    public void testUnburyBuffer () {
        LispObject lastBuffer = evaluateString("(last-buffer)");
        LispObject unburiedBuffer = evaluateString("(unbury-buffer)");
        Assert.assertEquals(lastBuffer, unburiedBuffer);
    }

    @Test
    public void testGetBufferCreateByName() {
        LispObject lispObject = evaluateString("(get-buffer-create \"1.txt\")");
        Assert.assertEquals(myTests.get("1.txt"), lispObject);
    }

    @Test
    public void testGetBufferCreateByBuffer() {
        LispObject lispObject = evaluateString("(get-buffer-create (current-buffer))");
        Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), lispObject);
    }

//    @Test
//    public void testGetBufferCreate_NonExistent() {
//        LispObject lispObject = evaluateString("(get-buffer-create \"test.txt\")");
//        myEnvironment.createBuffer("test.txt");
//        Assert.assertEquals(myEnvironment.createBuffer("test.txt"), lispObject);
//    }

    @Test
    public void testGenerateNewBufferName () {
        LispObject name = evaluateString("(generate-new-buffer-name \"1.txt\")");
        Assert.assertEquals(new LispString("1.txt<2>"), name);
        name = evaluateString("(generate-new-buffer-name \"5.txt\")");
        Assert.assertEquals(new LispString("5.txt"), name);
        name = evaluateString("(generate-new-buffer-name \"1.txt\" \"1.txt\")");
        Assert.assertEquals(new LispString("1.txt"), name);
    }

    @Test
    public void testGenerateNewBuffer () {
        evaluateString("(generate-new-buffer \"1.txt\")");
    }

    @Test
    public void testGenerateNewBufferDouble () {
        try {
            evaluateString("(generate-new-buffer \"1.txt\")");
            myEnvironment.createBuffer("1.txt<2>");
        } catch (Exception e) {
            Assert.assertEquals("double 1.txt<2>", getCause(e).getMessage());
        }
    }

    @Test
    public void testAlivePredicate () {
        LispObject lispObject = evaluateString("(buffer-live-p (get-buffer \"1.txt\"))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(kill-buffer \"1.txt\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = evaluateString("(buffer-live-p (get-buffer \"1.txt\"))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = evaluateString("(buffer-live-p \"1.txt\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testReplaceBufferInWindows () {
        LispObject lispObject = evaluateString("(replace-buffer-in-windows)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), myTests.get(myTestFiles[1]));
    }

    @Test
    public void testKillBuffer () {
        LispObject lispObject = evaluateString("(kill-buffer \"3.txt\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = myEnvironment.getBufferCurrentForEditing();
        Assert.assertEquals(myEnvironment.findBufferSafe(myTestFiles[1]), lispObject);
        Assert.assertEquals(myEnvironment.getBuffersSize(), 2);
        Assert.assertTrue(myEnvironment.isBufferDead("3.txt"));
    }

    @Test
    public void testBufferEnd () {
        LispObject bufferEnd = evaluateString("(buffer-end 1)");
        LispObject pos = evaluateString("(point-max)");
        Assert.assertEquals(pos, bufferEnd);
        bufferEnd = evaluateString("(buffer-end 0)");
        pos = evaluateString("(point-min)");
        Assert.assertEquals(pos, bufferEnd);
        bufferEnd = evaluateString("(buffer-end -1)");
        Assert.assertEquals(pos, bufferEnd);
    }

    @Test
    public void testBufferEndWrongArg () {
        try {
            evaluateString("(buffer-end 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument number-or-marker-p a)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testDefaultValueGlobals() {
        LispObject r = evaluateString("(default-value 'default-directory)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = evaluateString("(default-value 'mark-active)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    //todo:simple.el required
//    @Test
//    public void testDefaultValueExtern() {
//        LispObject r = evaluateString("(default-value 'mark-ring)");
//        Assert.assertEquals(LispSymbol.ourNil, r);
//    }

    @Test
    public void testDefaultValueBufferLocals() {
        try {
            evaluateString("(default-value 'is-alive)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-variable is-alive)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
}


