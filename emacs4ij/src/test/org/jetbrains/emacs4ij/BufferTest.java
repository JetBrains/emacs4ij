package org.jetbrains.emacs4ij;

import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
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
    Parser myParser = new Parser();
    String myTestsPath = "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    HashMap<String, IdeaBuffer> myTests;
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        GlobalEnvironment.ourEmacsSource = "/home/kate/Downloads/emacs 23.2a/emacs-23.2";
        GlobalEnvironment.ourEmacsPath = "/usr/share/emacs/23.2";
        super.setUp();
        myTestFiles = (new File (myTestsPath)).list();
        myTests = new HashMap<String, IdeaBuffer>();

        GlobalEnvironment.initialize(new BufferCreator(), new IdeProvider());
        //GlobalEnvironment.getInstance().startRecording();

        //GlobalEnvironment.setProject(myFixture.getProject());
        myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
       // GlobalEnvironment.getInstance().clearRecorded();


        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            IdeaBuffer buffer = new IdeaBuffer(myEnvironment, fileName, myTestsPath, getEditor());
            myTests.put(fileName, buffer);
            myEnvironment.defineBuffer(buffer);
        }

//        LispFrame current  = new IdeaFrame(new IdeFrameImpl(null, null, null, null, null, null));

   //     LispFrame current  = new IdeaFrame(WindowManager.getInstance().getIdeFrame(myFixture.getProject()));
  /*      GlobalEnvironment.onFrameOpened(current);
        GlobalEnvironment.setSelectedFrame(current);*/
    }

    private Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    private LObject eval (String lispCode) {
        return myParser.parseLine(lispCode).evaluate(myEnvironment);
    }

    @Test
    public void testCurrentBuffer() {
        LObject lispObject = eval("(current-buffer)");
        Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), lispObject);
    }

    @Test
    public void testBufferp() {
        LObject lispObject = eval("(bufferp (current-buffer))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testBufferpWrongNargs() {
        try {
            eval("(bufferp)");
        } catch (WrongNumberOfArgumentsException e) {
            //success
        }
    }

    @Test
    public void testGetBufferByName() {
        LObject lispObject = eval("(get-buffer \"1.txt\")");
        Assert.assertEquals(myTests.get("1.txt"), lispObject);
    }

    @Test
    public void testGetBufferByBuffer() {
        LObject lispObject = eval("(get-buffer (current-buffer))");
        Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), lispObject);
    }

    @Test
    public void testGetBuffer_NonExistent() {
        LObject lispObject = eval("(get-buffer \"test.txt\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testGetBuffer_Nil() {
        try {
            eval("(get-buffer nil)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
        }
    }

    @Test
    public void testSetBufferWrongType() {
        try {
            eval("(set-buffer 5)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
        }
    }

    @Test
    public void testBufferSize() {
        LObject currentBufferSizeAnonymous = eval("(buffer-size)");
        LObject currentBufferSize = eval("(buffer-size (current-buffer))");
        Assert.assertEquals(currentBufferSize, currentBufferSizeAnonymous);
    }

    @Test
    public void testBufferSizeNil() {
        LObject currentBufferSizeAnonymous = eval("(buffer-size nil)");
        LObject currentBufferSize = eval("(buffer-size (current-buffer))");
        Assert.assertEquals(currentBufferSize, currentBufferSizeAnonymous);
    }

    @Test
    public void testBufferName () {
        LObject lispObject = eval("(buffer-name (get-buffer \"1.txt\"))");
        Assert.assertEquals(new LispString("1.txt"), lispObject);
    }

    @Test
    public void testBufferName_Nil () {
        LObject currentBufferName = eval("(buffer-name (current-buffer))");
        LObject lispObject = eval("(buffer-name)");
        Assert.assertEquals(currentBufferName, lispObject);
        lispObject = eval("(buffer-name nil)");
        Assert.assertEquals(currentBufferName, lispObject);
    }

    @Test
    public void testOtherBuffer_SingleBuffer () {
        while (myEnvironment.getBuffersSize() != 1)
            myEnvironment.closeCurrentBuffer();
        LObject lispObject = eval("(other-buffer)");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
    }

    @Test
    public void testOtherBuffer_NoBuffers () {
        myEnvironment.closeAllBuffers();
        try {
            eval("(other-buffer)");
        } catch (RuntimeException e) {
            Assert.assertEquals(new NoOpenedBufferException().getMessage(), getCause(e).getMessage());
            return;
        }
        Assert.assertEquals(1, 0);
    }

    @Test
    public void testOtherBuffer_NoParameters3buffers () {
        LObject lispObject = eval("(other-buffer)");
        Assert.assertEquals(myEnvironment.getBufferByIndex(myEnvironment.getBuffersSize()-2), lispObject);
    }

    @Test
    public void testOtherBuffer_NotBufferParameter () {
        LObject lispObject = eval("(other-buffer 1)");
        Assert.assertEquals(myEnvironment.getBufferByIndex(myEnvironment.getBuffersSize()-2), lispObject);
    }

    @Test
    public void testOtherBuffer_BufferParameter () {
        LObject lispObject = eval("(other-buffer (get-buffer \""+ myTestFiles[0] +"\" ))");
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
            eval("(switch-to-buffer)");
        } catch (WrongNumberOfArgumentsException e) {
            //success
            return;
        }
        Assert.assertEquals(0, 1);
    }

    @Test
    public void testSwitchToBuffer_Nil() {
        LObject lispObject = eval("(switch-to-buffer nil)");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_ExistentString() {
        LObject lispObject = eval("(switch-to-buffer \"" + myTestFiles[0] + "\")");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_NonExistentString() {
        LObject lispObject = eval("(switch-to-buffer \"test.txt\")");
        Assert.assertEquals(new LispString("It is not allowed to create files this way."), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_Buffer() {
        LObject lispObject = eval("(switch-to-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSwitchToBuffer_ExistentString_NoRecord() {
        LObject lispObject = eval("(switch-to-buffer \"" + myTestFiles[0] + "\" 5)");
        Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        Assert.assertEquals(myEnvironment.getBufferByIndex(0).getEditor() , fileEditorManager.getSelectedTextEditor());
    }

    @Test
    public void testSetBuffer_String () {
        LObject lispObject = eval("(set-buffer \"" + myTestFiles[0] + "\")");
        Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
    }

    @Test
    public void testSetBuffer_Buffer () {
        LObject lispObject = eval("(set-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
        Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
    }

    @Test
    public void testSetBuffer_InProgn () {
        LObject buffer = eval("(progn (set-buffer \"" + myTestFiles[0] + "\") (buffer-name))");
        Assert.assertEquals(new LispString(myTestFiles[0]), buffer);
        eval("(set-buffer \"" + myTestFiles[0] + "\")");
        buffer = eval("(buffer-name)");
        Assert.assertEquals(new LispString(myTestFiles[myTestFiles.length-1]), buffer);
    }

    @Test
    public void testSetBuffer_WrongParameter () {
        try {
            eval("(set-buffer 5)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgumentException);
        }
    }

    @Test
    public void testPoint() {
        LObject lispObject = eval("(point)");
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        int point = fileEditorManager.getSelectedTextEditor().logicalPositionToOffset(fileEditorManager.getSelectedTextEditor().getCaretModel().getLogicalPosition()) + 1;
        Assert.assertEquals(new LispInteger(point), lispObject);
    }

    @Test
    public void testPointMin() {
        LObject lispObject = eval("(point-min)");
        Assert.assertEquals(new LispInteger(1), lispObject);
    }

    @Test
    public void testPointMax() {
        LObject lispObject = eval("(point-max)");
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(myFixture.getProject());
        int pointMax = fileEditorManager.getSelectedTextEditor().getDocument().getTextLength()+1;
        Assert.assertEquals(new LispInteger(pointMax), lispObject);
    }

    @Test
    public void testGoToChar () {
        LObject lispObject = eval("(goto-char 5)");
        Assert.assertEquals(new LispInteger(5), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().point(), 5);
    }

    @Test
    public void testGoToChar_End () {
        LObject lispObject = eval("(goto-char 50)");
        Assert.assertEquals(new LispInteger(50), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testGoToChar_Begin () {
        LObject lispObject = eval("(goto-char -50)");
        Assert.assertEquals(new LispInteger(-50), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar_NoParam () {
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LObject lispObject = eval("(forward-char)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from+1, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar () {
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LObject lispObject = eval("(forward-char 5)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from+5, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar_End () {
        LObject lispObject = eval("(forward-char 50)");
        Assert.assertEquals(new LispSymbol("End of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testForwardChar_Begin () {
        LObject lispObject = eval("(forward-char -50)");
        Assert.assertEquals(new LispSymbol("Beginning of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar_NoParam () {
        myEnvironment.getBufferCurrentForEditing().forwardChar(2);
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LObject lispObject = eval("(backward-char)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from-1, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar () {
        myEnvironment.getBufferCurrentForEditing().forwardChar(3);
        int from = myEnvironment.getBufferCurrentForEditing().point();
        LObject lispObject = eval("(backward-char 2)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(from-2, myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar_End () {
        LObject lispObject = eval("(backward-char 50)");
        Assert.assertEquals(new LispSymbol("Beginning of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
    }

    @Test
    public void testBackwardChar_Begin () {
        LObject lispObject = eval("(backward-char -50)");
        Assert.assertEquals(new LispSymbol("End of buffer"), lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
    }

    //todo:: test mySymbols.put("buffer-end", new LispSymbol("buffer-end", LispSymbol.FunctionType.BuiltIn)); //note: it is compiled lisp function in emacs

    //==== markers ====

    @Test
    public void testPointMarkerOk() {
        LObject lispObject = eval("(point-marker)");
        LispBuffer currentBuffer = (LispBuffer) eval("(current-buffer)");
        LispMarker marker = new LispMarker(currentBuffer.point(), currentBuffer);
        Assert.assertEquals(marker, lispObject);
    }

    @Test
    public void testPointMarkerInvalid () {
        try {
            eval("(point-marker)");
        } catch (WrongNumberOfArgumentsException e) {
            //success
        }
    }

    @Test
    public void testPointMinMarker () {
        LObject lispObject = eval("(point-min-marker)");
        LispBuffer currentBuffer = (LispBuffer) eval("(current-buffer)");
        Assert.assertEquals(new LispMarker(currentBuffer.pointMin(), currentBuffer), lispObject);
    }

    @Test
    public void testPointMaxMarker () {
        LObject lispObject = eval("(point-max-marker)");
        LispBuffer currentBuffer = (LispBuffer) eval("(current-buffer)");
        Assert.assertEquals(new LispMarker(currentBuffer.pointMax(), currentBuffer), lispObject);
    }

    @Test
    public void testCopyMarker_Marker () {
        eval("(defvar m1 (make-marker))");
        LObject lispObject = eval("(copy-marker m1)");
        LispMarker m1 = new LispMarker();
        Assert.assertEquals(m1, lispObject);

        lispObject = eval("(copy-marker m1 nil)");
        Assert.assertEquals(m1, lispObject);

        lispObject = eval("(copy-marker 50000 nil)");
        LispBuffer buffer = myEnvironment.getBufferCurrentForEditing();
        m1 = new LispMarker(buffer.pointMax(), buffer);
        Assert.assertEquals(m1, lispObject);

        m1.setPosition(buffer.pointMin());
        m1.setInsertionType(LispSymbol.ourT);
        lispObject = eval("(copy-marker -50 t)");
        Assert.assertEquals(m1, lispObject);
    }

    @Test
    public void testEq() {
        LObject lispObject = eval("(eq (point-marker) (point-marker))");
        junit.framework.Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = eval("(equal (point-marker) (point-marker))");
        junit.framework.Assert.assertEquals(LispSymbol.ourT, lispObject);
    }

    @Test
    public void testDefaultDirectory () {
        LObject lispObject = eval("default-directory");
        Assert.assertEquals(new LispString(myTestsPath), lispObject);
    }

    @Test
    public void testBufferList () {
        LObject lispObject = eval("(buffer-list)");
        Assert.assertEquals(myEnvironment.getBufferList(), lispObject);
    }

    @Test
    public void testBuryBuffer_Current () {
        LispBuffer current = myEnvironment.getBufferCurrentForEditing();
        ArrayList<LispBuffer> buffers = myEnvironment.getBuffers();
        buffers.remove(current);
        buffers.add(0, current);
        eval("(bury-buffer)");
        Assert.assertEquals(buffers, myEnvironment.getBuffers());
    }

    @Test
    public void testBuryBuffer_String () {
        LispBuffer current = myEnvironment.findBufferSafe(myTestFiles[1]);
        ArrayList<LispBuffer> buffers = myEnvironment.getBuffers();
        buffers.remove(current);
        buffers.add(0, current);
        eval("(bury-buffer \"" + myTestFiles[1] + "\")");
        Assert.assertEquals(buffers, myEnvironment.getBuffers());
    }

    @Test
    public void testBuryBuffer_Buffer () {
        LispBuffer current = myEnvironment.findBufferSafe(myTestFiles[1]);
        ArrayList<LispBuffer> buffers = myEnvironment.getBuffers();
        buffers.remove(current);
        buffers.add(0, current);
        eval("(bury-buffer (get-buffer \"" + myTestFiles[1] + "\"))");
        Assert.assertEquals(buffers, myEnvironment.getBuffers());
    }

    @Test
    public void testLastBuffer () {
        try {
            LObject lastBuffer = eval("(last-buffer)");
            Assert.assertTrue(lastBuffer instanceof LispBuffer);
            Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
        } catch (Exception e) {
            System.out.println(getCause(e).getMessage());
        }
    }

    @Test
    public void testLastBuffer_Integer () {
        LObject lastBuffer = eval("(last-buffer 1)");
        Assert.assertTrue(lastBuffer instanceof LispBuffer);
        Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
    }

    @Test
    public void testLastBuffer_Other () {
        LObject lastBuffer = eval("(last-buffer (get-buffer \"" + myTestFiles[myTestFiles.length-1] + "\"))");
        Assert.assertTrue(lastBuffer instanceof LispBuffer);
        Assert.assertEquals(myTestFiles[myTestFiles.length-2], ((LispBuffer) lastBuffer).getName());
        lastBuffer = eval("(last-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
        Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
    }

    @Test
    public void testUnburyBuffer () {
        LObject lastBuffer = eval("(last-buffer)");
        LObject unburiedBuffer = eval("(unbury-buffer)");
        Assert.assertEquals(lastBuffer, unburiedBuffer);
    }

    @Test
    public void testGetBufferCreateByName() {
        LObject lispObject = eval("(get-buffer-create \"1.txt\")");
        Assert.assertEquals(myTests.get("1.txt"), lispObject);
    }

    @Test
    public void testGetBufferCreateByBuffer() {
        LObject lispObject = eval("(get-buffer-create (current-buffer))");
        Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), lispObject);
    }

    @Test
    public void testGetBufferCreate_NonExistent() {
        LObject lispObject = eval("(get-buffer-create \"test.txt\")");
        myEnvironment.createBuffer("test.txt");
        Assert.assertEquals(myEnvironment.createBuffer("test.txt"), lispObject);
    }

    @Test
    public void testGenerateNewBufferName () {
        LObject name = eval("(generate-new-buffer-name \"1.txt\")");
        Assert.assertEquals(new LispString("1.txt<2>"), name);
        name = eval("(generate-new-buffer-name \"5.txt\")");
        Assert.assertEquals(new LispString("5.txt"), name);
        name = eval("(generate-new-buffer-name \"1.txt\" \"1.txt\")");
        Assert.assertEquals(new LispString("1.txt"), name);
    }

    @Test
    public void testGenerateNewBuffer () {
        try {
            LObject newBuffer = eval("(generate-new-buffer \"1.txt\")");
            LispBuffer buffer = myEnvironment.createBuffer("1.txt<2>");
            Assert.assertEquals(buffer, newBuffer);
        } catch (Exception e) {
            Assert.fail(getCause(e).getMessage());
        }
    }

    @Test
    public void testAlivePredicate () {
        LObject lispObject = eval("(buffer-live-p (get-buffer \"1.txt\"))");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = eval("(kill-buffer \"1.txt\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = eval("(buffer-live-p (get-buffer \"1.txt\"))");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        lispObject = eval("(buffer-live-p \"1.txt\")");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
    }

    @Test
    public void testReplaceBufferInWindows () {
        LObject lispObject = eval("(replace-buffer-in-windows)");
        Assert.assertEquals(LispSymbol.ourNil, lispObject);
        Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), myTests.get(myTestFiles[1]));
    }

    @Test
    public void testKillBuffer () {
        LObject lispObject = eval("(kill-buffer \"3.txt\")");
        Assert.assertEquals(LispSymbol.ourT, lispObject);
        lispObject = myEnvironment.getBufferCurrentForEditing();
        Assert.assertEquals(myEnvironment.findBufferSafe(myTestFiles[1]), lispObject);
        Assert.assertEquals(myEnvironment.getBuffersSize(), 2);
        Assert.assertTrue(myEnvironment.isBufferDead("3.txt"));
    }

    @Test
    public void testBufferEnd () {
        LObject bufferEnd = eval("(buffer-end 1)");
        LObject pos = eval("(point-max)");
        Assert.assertEquals(pos, bufferEnd);
        bufferEnd = eval("(buffer-end 0)");
        pos = eval("(point-min)");
        Assert.assertEquals(pos, bufferEnd);
        bufferEnd = eval("(buffer-end -1)");
        Assert.assertEquals(pos, bufferEnd);
    }

    @Test
    public void testBufferEndWrongArg () {
        try {
            eval("(buffer-end 'a)");
        } catch (Exception e) {
            Assert.assertEquals("'(wrong-type-argument number-or-marker-p a)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
    
    @Test
    public void testDefaultValueGlobals() {
        LObject r = eval("(default-value 'default-directory)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = eval("(default-value 'mark-active)");
        Assert.assertEquals(LispSymbol.ourNil, r);
        r = eval("(default-value 'mark-ring)");
        Assert.assertEquals(LispSymbol.ourNil, r);
    }

    @Test
    public void testDefaultValueBufferLocals() {
        try {
            eval("(default-value 'is-alive)");
        } catch (Exception e) {
            Assert.assertEquals("'(void-variable is-alive)", getCause(e).getMessage());
            return;
        }
        Assert.fail();
    }
}


