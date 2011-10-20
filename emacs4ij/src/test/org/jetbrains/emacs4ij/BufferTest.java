package org.jetbrains.emacs4ij;

import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.HashMap;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 9/22/11
 * Time: 3:26 PM
 * To change this template use File | Settings | File Templates.
 */
public class BufferTest extends CodeInsightFixtureTestCase {
    Environment myEnvironment;
    Parser myParser = new Parser();
    String myTestsPath = "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    HashMap<String, IdeaEditor> myTests;
    String[]  myTestFiles;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        myTestFiles = (new File (myTestsPath)).list();
        myTests = new HashMap<String, IdeaEditor>();
        myEnvironment = new Environment(new Environment());
        for (String fileName: myTestFiles) {
            myFixture.configureByFile(myTestsPath + fileName);
            myTests.put(fileName, new IdeaEditor(fileName, myTestsPath, getEditor()));
            myEnvironment.defineBuffer(new IdeaEditor(fileName, myTestsPath, getEditor()));
        }
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
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgument);
        }
    }

    @Test
    public void testSetBufferWrongType() {
        try {
            eval("(set-buffer 5)");
            Assert.assertEquals(0, 1);
        } catch (Exception e) {
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgument);
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
            Assert.assertEquals("no buffer is currently opened", getCause(e).getMessage());
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
            Assert.assertTrue(getCause(e) instanceof WrongTypeArgument);
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
        Assert.assertEquals(new LispMarker(currentBuffer.point(), currentBuffer), lispObject);
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

}
