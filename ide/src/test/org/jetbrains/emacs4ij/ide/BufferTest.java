package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.fileEditor.FileEditorManager;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMarker;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.text.Action;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.subroutine.Buffer;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Match;
import org.jetbrains.emacs4ij.jelisp.subroutine.SpecialForms;
import org.junit.Assert;
import org.junit.Before;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class BufferTest extends IdeTestCase {
  private FileEditorManager myFileEditorManager;

  @Before
  public void setUp() throws Exception {
    super.setUp();
    setTestFiles(true);
    myFileEditorManager = FileEditorManager.getInstance(getProject());
  }

  public void testSetBufferNoBuffer() {
    try {
      Buffer.setBuffer(myEnvironment, new LispString("hello.lisp"));
    } catch (NoBufferException e) {
      //success
      return;
    }
    Assert.fail();
  }

  public void testCurrentBuffer() {
    LispObject lispObject = evaluateString("(current-buffer)");
    Assert.assertEquals(myTests.get(myTestFiles[0]), lispObject);
  }

  public void testBufferp() {
    LispObject lispObject = evaluateString("(bufferp (current-buffer))");
    Assert.assertEquals(LispSymbol.T, lispObject);
  }

  public void testBufferpWrongNargs() {
    try {
      evaluateString("(bufferp)");
    } catch (WrongNumberOfArgumentsException e) {
      //success
      return;
    }
    Assert.fail();
  }

  public void testGetBufferByName() {
    LispObject lispObject = evaluateString("(get-buffer \"1.txt\")");
    Assert.assertEquals(myTests.get("1.txt"), lispObject);
  }

  public void testGetBufferByBuffer() {
    LispObject lispObject = evaluateString("(get-buffer (current-buffer))");
    Assert.assertEquals(myTests.get(myTestFiles[0]), lispObject);
  }


  public void testGetBuffer_NonExistent() {
    LispObject lispObject = evaluateString("(get-buffer \"test.txt\")");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
  }

  public void testGetBuffer_Nil() {
    try {
      evaluateString("(get-buffer nil)");
      Assert.assertEquals(0, 1);
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument stringp nil)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testSetBufferWrongType() {
    try {
      evaluateString("(set-buffer 5)");
      Assert.assertEquals(0, 1);
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument stringp 5)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testBufferSize() {
    LispObject currentBufferSizeAnonymous = evaluateString("(buffer-size)");
    LispObject currentBufferSize = evaluateString("(buffer-size (current-buffer))");
    Assert.assertEquals(currentBufferSize, currentBufferSizeAnonymous);
  }

  public void testBufferSizeNil() {
    LispObject currentBufferSizeAnonymous = evaluateString("(buffer-size nil)");
    LispObject currentBufferSize = evaluateString("(buffer-size (current-buffer))");
    Assert.assertEquals(currentBufferSize, currentBufferSizeAnonymous);
  }


  public void testBufferName () {
    LispObject lispObject = evaluateString("(buffer-name (get-buffer \"1.txt\"))");
    Assert.assertEquals(new LispString("1.txt"), lispObject);
  }


  public void testBufferName_Nil () {
    LispObject currentBufferName = evaluateString("(buffer-name (current-buffer))");
    LispObject lispObject = evaluateString("(buffer-name)");
    Assert.assertEquals(currentBufferName, lispObject);
    lispObject = evaluateString("(buffer-name nil)");
    Assert.assertEquals(currentBufferName, lispObject);
  }

  public void testOtherBuffer_SingleBuffer () {
    while (myEnvironment.getBuffersSize() != 1)
      myEnvironment.closeCurrentBuffer();
    LispObject lispObject = evaluateString("(other-buffer)");
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
  }


  public void testOtherBuffer_NoBuffers () {
    myEnvironment.closeAllBuffers();
    try {
      evaluateString("(other-buffer)");
    } catch (RuntimeException e) {
      Assert.assertEquals(new NoOpenedBufferException().getMessage(), getCauseMsg(e));
      return;
    }
    Assert.assertEquals(1, 0);
  }

  public void testOtherBuffer_NoParameters3buffers () {
    LispObject lispObject = evaluateString("(other-buffer)");
    Assert.assertEquals(myEnvironment.getBufferByIndex(1), lispObject);
  }

  public void testOtherBuffer_NotBufferParameter () {
    LispObject lispObject = evaluateString("(other-buffer 1)");
    Assert.assertEquals(myEnvironment.getBufferByIndex(1), lispObject);
  }

  public void testOtherBuffer_BufferParameter () {
    LispObject lispObject = evaluateString("(other-buffer (get-buffer \"" + myTestFiles[1] + "\" ))");
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
  }

  public void testEnvironment_GetBuffersNames() {
    String[] buffersNames = myEnvironment.getBuffersNames();
    int size = myTestFiles.length + 1;
    String[] testFiles = new String[size];
    System.arraycopy(myTestFiles, 0, testFiles, 0, myTestFiles.length);
    testFiles[size - 1] = TestIdeaMinibuffer.getInstance().getName();
    Assert.assertArrayEquals(testFiles, buffersNames);
  }

  public void testSwitchToBuffer_NoArgs() {
    try {
      evaluateString("(switch-to-buffer)");
    } catch (WrongNumberOfArgumentsException e) {
      //success
      return;
    }
    Assert.assertEquals(0, 1);
  }


  public void testSwitchToBuffer_Nil() {
    LispObject lispObject = evaluateString("(switch-to-buffer nil)");
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
    Assert.assertEquals(((IdeaBuffer)myEnvironment.getBufferCurrentForEditing()).getEditor() , myFileEditorManager.getSelectedTextEditor());
  }

  public void testSwitchToBuffer_ExistentString() {
    LispObject lispObject = evaluateString("(switch-to-buffer \"" + myTestFiles[0] + "\")");
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
    Assert.assertEquals(((IdeaBuffer)myEnvironment.getBufferCurrentForEditing()).getEditor(), myFileEditorManager.getSelectedTextEditor());
  }


  public void testSwitchToBuffer_NonExistentString() {
    LispObject lispObject = evaluateString("(switch-to-buffer \"test.txt\")");
    Assert.assertEquals(new LispString("It is not allowed to create files this way."), lispObject);
    Assert.assertEquals(((IdeaBuffer)myEnvironment.getBufferCurrentForEditing()).getEditor(), myFileEditorManager.getSelectedTextEditor());
  }


  public void testSwitchToBuffer_Buffer() {
    LispObject lispObject = evaluateString("(switch-to-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), lispObject);
    Assert.assertEquals(((IdeaBuffer)myEnvironment.getBufferCurrentForEditing()).getEditor(), myFileEditorManager.getSelectedTextEditor());
  }


  public void testSwitchToBuffer_ExistentString_NoRecord() {
    LispObject lispObject = evaluateString("(switch-to-buffer \"" + myTestFiles[0] + "\" 5)");
    Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
    Assert.assertEquals(((IdeaBuffer)myEnvironment.getBufferByIndex(0)).getEditor(), myFileEditorManager.getSelectedTextEditor());
  }


  public void testSetBuffer_String () {
    LispObject lispObject = evaluateString("(set-buffer \"" + myTestFiles[0] + "\")");
    Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
  }


  public void testSetBuffer_Buffer () {
    LispObject lispObject = evaluateString("(set-buffer (get-buffer \"" + myTestFiles[0] + "\"))");
    Assert.assertEquals(myEnvironment.getBufferByIndex(0), lispObject);
  }


  public void testSetBuffer_InProgn () {
    LispObject buffer = evaluateString("(progn (set-buffer \"" + myTestFiles[1] + "\") (buffer-name))");
    Assert.assertEquals(new LispString(myTestFiles[1]), buffer);
    evaluateString("(set-buffer \"" + myTestFiles[1] + "\")");
    buffer = evaluateString("(buffer-name)");
    Assert.assertEquals(new LispString(myTestFiles[0]), buffer);
  }

  public void testPoint() {
    LispObject lispObject = evaluateString("(point)");
    int point = myFileEditorManager.getSelectedTextEditor().logicalPositionToOffset(myFileEditorManager.getSelectedTextEditor().getCaretModel().getLogicalPosition()) + 1;
    Assert.assertEquals(new LispInteger(point), lispObject);
  }

  public void testPointMin() {
    LispObject lispObject = evaluateString("(point-min)");
    Assert.assertEquals(new LispInteger(1), lispObject);
  }

  public void testPointMax() {
    LispObject lispObject = evaluateString("(point-max)");
    int pointMax = myFileEditorManager.getSelectedTextEditor().getDocument().getTextLength()+1;
    Assert.assertEquals(new LispInteger(pointMax), lispObject);
  }

  public void testGoToChar () {
    LispObject lispObject = evaluateString("(goto-char 5)");
    Assert.assertEquals(new LispInteger(5), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().point(), 5);
  }

  public void testGoToChar_End () {
    LispObject lispObject = evaluateString("(goto-char 200)");
    Assert.assertEquals(new LispInteger(200), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testGoToChar_Begin () {
    LispObject lispObject = evaluateString("(goto-char -50)");
    Assert.assertEquals(new LispInteger(-50), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testForwardChar_NoParam () {
    int from = myEnvironment.getBufferCurrentForEditing().point();
    LispObject lispObject = evaluateString("(forward-char)");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
    Assert.assertEquals(from+1, myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testForwardChar () {
    int from = myEnvironment.getBufferCurrentForEditing().point();
    LispObject lispObject = evaluateString("(forward-char 5)");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
    Assert.assertEquals(from+5, myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testForwardChar_End () {
    LispObject lispObject = evaluateString("(forward-char 200)");
    Assert.assertEquals(new LispSymbol("End of buffer"), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testForwardChar_Begin () {
    LispObject lispObject = evaluateString("(forward-char -50)");
    Assert.assertEquals(new LispSymbol("Beginning of buffer"), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testBackwardChar_NoParam () {
    myEnvironment.getBufferCurrentForEditing().forwardChar(2);
    int from = myEnvironment.getBufferCurrentForEditing().point();
    LispObject lispObject = evaluateString("(backward-char)");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
    Assert.assertEquals(from-1, myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testBackwardChar () {
    myEnvironment.getBufferCurrentForEditing().forwardChar(3);
    int from = myEnvironment.getBufferCurrentForEditing().point();
    LispObject lispObject = evaluateString("(backward-char 2)");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
    Assert.assertEquals(from-2, myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testBackwardChar_End () {
    LispObject lispObject = evaluateString("(backward-char 50)");
    Assert.assertEquals(new LispSymbol("Beginning of buffer"), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMin(), myEnvironment.getBufferCurrentForEditing().point());
  }

  public void testBackwardChar_Begin () {
    LispObject lispObject = evaluateString("(backward-char -500)");
    Assert.assertEquals(new LispSymbol("End of buffer"), lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing().pointMax(), myEnvironment.getBufferCurrentForEditing().point());
  }

  //==== markers ====

  public void testPointMarkerOk() {
    LispObject lispObject = evaluateString("(point-marker)");
    LispBuffer currentBuffer = (LispBuffer) evaluateString("(current-buffer)");
    LispMarker marker = new LispMarker(currentBuffer.point(), currentBuffer);
    Assert.assertEquals(marker, lispObject);
  }

  public void testPointMarkerInvalid () {
    try {
      evaluateString("(point-marker)");
    } catch (WrongNumberOfArgumentsException e) {
      //success
    }
  }

  public void testPointMinMarker () {
    LispObject lispObject = evaluateString("(point-min-marker)");
    LispBuffer currentBuffer = (LispBuffer) evaluateString("(current-buffer)");
    Assert.assertEquals(new LispMarker(currentBuffer.pointMin(), currentBuffer), lispObject);
  }

  public void testPointMaxMarker () {
    LispObject lispObject = evaluateString("(point-max-marker)");
    LispBuffer currentBuffer = (LispBuffer) evaluateString("(current-buffer)");
    Assert.assertEquals(new LispMarker(currentBuffer.pointMax(), currentBuffer), lispObject);
  }

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

    m1.set(buffer.pointMin(), buffer);
    m1.setInsertionType(LispSymbol.T);
    lispObject = evaluateString("(copy-marker -50 t)");
    Assert.assertEquals(m1, lispObject);
  }


  public void testEq() {
    LispObject lispObject = evaluateString("(eq (point-marker) (point-marker))");
    junit.framework.Assert.assertEquals(LispSymbol.NIL, lispObject);
    lispObject = evaluateString("(equal (point-marker) (point-marker))");
    junit.framework.Assert.assertEquals(LispSymbol.T, lispObject);
  }


  public void testDefaultDirectory () {
    LispString lispObject = (LispString) new LispSymbol("default-directory")
        .evaluate(myEnvironment.getBufferCurrentForEditing().getEnvironment());
    //evaluateString("default-directory");
    Assert.assertTrue(lispObject.getData().endsWith(myTestsPath));
  }


  public void testBufferList () {
    LispObject lispObject = evaluateString("(buffer-list)");
    Assert.assertEquals(myEnvironment.getBufferList(), lispObject);
    Assert.assertEquals(myTests.get(myTestFiles[0]), ((LispList)lispObject).car());
  }


  public void testBufferListFrame () {
    LispObject lispObject = evaluateString("(buffer-list (selected-frame))");
    Assert.assertEquals(myEnvironment.getBufferList(), lispObject);
    Assert.assertEquals(myTests.get(myTestFiles[0]), ((LispList) lispObject).car());
  }

  public void testBuryBuffer_Current () {
    LispBuffer current = myEnvironment.getBufferCurrentForEditing();
    List<LispBuffer> buffers = buryCurrent(current);
    evaluateString("(bury-buffer)");
    Assert.assertEquals(buffers, myEnvironment.getBuffers());
  }

  public void testBuryBuffer_String () {
    LispBuffer current = myEnvironment.findBufferSafe(myTestFiles[1]);
    List<LispBuffer> buffers = buryCurrent(current);
    evaluateString("(bury-buffer \"" + myTestFiles[1] + "\")");
    Assert.assertEquals(buffers, myEnvironment.getBuffers());
  }

  public void testBuryBuffer_Buffer () {
    LispBuffer current = myEnvironment.findBufferSafe(myTestFiles[1]);
    List<LispBuffer> buffers = buryCurrent(current);
    evaluateString("(bury-buffer (get-buffer \"" + myTestFiles[1] + "\"))");
    Assert.assertEquals(buffers, myEnvironment.getBuffers());
  }

  private List<LispBuffer> buryCurrent(LispBuffer current) {
    List<LispBuffer> buffers = new ArrayList<>(myEnvironment.getBuffers());
    buffers.remove(current);
    buffers.add(current);
    return buffers;
  }

  public void testGetNextValidBuffer() {
    myEnvironment.hideBuffer(myTests.get(myTestFiles[1]));

    LispObject t = evaluateString("(null (get-buffer-window (get-buffer \"" + myTestFiles[1] + "\") 'visible))");
    Assert.assertEquals(LispSymbol.T, t);

    LispObject buffer = evaluateString("(get-next-valid-buffer (nreverse (buffer-list)))");
    Assert.assertTrue(buffer instanceof LispBuffer);
    Assert.assertEquals(myTests.get(myTestFiles[1]), buffer);
  }


  public void testGetNextValidBufferNil() {
    LispObject buffer = evaluateString("(get-next-valid-buffer (buffer-list))");
    Assert.assertEquals(LispSymbol.NIL, buffer);
  }


  public void testGetNextValidBufferVisibleOk() {
    LispObject buffer = evaluateString("(get-next-valid-buffer (buffer-list) 1 t)");
    Assert.assertEquals(myTests.get(myTestFiles[0]), buffer);
    buffer = evaluateString("(get-next-valid-buffer (nreverse (buffer-list)) 1 t)");
    Assert.assertEquals(myTests.get(myTestFiles[myTestFiles.length - 1]), buffer);
  }


  public void testLastBuffer () {
    myEnvironment.hideBuffer(myTests.get(myTestFiles[1]));
    LispObject lastBuffer = evaluateString("(last-buffer)");
    Assert.assertTrue(lastBuffer instanceof LispBuffer);
    Assert.assertEquals(myTests.get(myTestFiles[1]), lastBuffer);
  }


  public void testLastBuffer_Integer () {
    LispObject lastBuffer = evaluateString("(last-buffer 1 t)");
    Assert.assertTrue(lastBuffer instanceof LispBuffer);
    Assert.assertEquals(myTestFiles[myTestFiles.length - 1], ((LispBuffer) lastBuffer).getName());
  }


  public void testLastBuffer_Other () {
    LispObject lastBuffer = evaluateString("(last-buffer (get-buffer \"" + myTestFiles[myTestFiles.length - 1] + "\") t)");
    Assert.assertTrue(lastBuffer instanceof LispBuffer);
    Assert.assertEquals(myTestFiles[myTestFiles.length-2], ((LispBuffer) lastBuffer).getName());
    lastBuffer = evaluateString("(last-buffer (get-buffer \"" + myTestFiles[0] + "\") t)");
    Assert.assertEquals(myTestFiles[myTestFiles.length-1], ((LispBuffer) lastBuffer).getName());
  }


  public void testUnburyBuffer () {
    myEnvironment.hideBuffer(myTests.get(myTestFiles[1]));
    LispObject lastBuffer = evaluateString("(last-buffer)");
    LispObject unburiedBuffer = evaluateString("(unbury-buffer)");
    Assert.assertEquals(lastBuffer, unburiedBuffer);
  }


  public void testGetBufferCreateByName() {
    LispObject lispObject = evaluateString("(get-buffer-create \"1.txt\")");
    Assert.assertEquals(myTests.get("1.txt"), lispObject);
  }


  public void testGetBufferCreateByBuffer() {
    LispObject lispObject = evaluateString("(get-buffer-create (current-buffer))");
    Assert.assertEquals(myTests.get(myTestFiles[0]), lispObject);
  }

  public void testGenerateNewBufferName () {
    LispObject name = evaluateString("(generate-new-buffer-name \"1.txt\")");
    Assert.assertEquals(new LispString("1.txt<2>"), name);
    name = evaluateString("(generate-new-buffer-name \"6.txt\")");
    Assert.assertEquals(new LispString("6.txt"), name);
    name = evaluateString("(generate-new-buffer-name \"1.txt\" \"1.txt\")");
    Assert.assertEquals(new LispString("1.txt"), name);
  }

//    todo: implement get-buffer-create
//    public void testGetBufferCreate_NonExistent() {
//        LispObject lispObject = evaluateString("(get-buffer-create \"test.txt\")");
//        myEnvironment.createBuffer("test.txt");
//        Assert.assertEquals(myEnvironment.createBuffer("test.txt"), lispObject);
//    }
//
//    public void testGenerateNewBuffer () {
//        int n = myEnvironment.getFrameBuffers(myEnvironment.getSelectedFrame()).length;
//        LispObject buffer = evaluateString("(generate-new-buffer \"1.txt\")");
//        Assert.assertTrue(buffer instanceof LispBuffer);
//        Assert.assertEquals("1.txt<2>", ((LispBuffer) buffer).getName());
//        Assert.assertEquals(n + 1, myEnvironment.getFrameBuffers(myEnvironment.getSelectedFrame()).length);
//    }
//
//    public void testGenerateNewBufferDouble () {
//        try {
//            evaluateString("(generate-new-buffer \"1.txt\")");
//            myEnvironment.createBuffer("1.txt<2>");
//        } catch (Exception e) {
//            Assert.assertEquals("Double buffer: 1.txt<2>", TestSetup.getCause(e));
//        }
//    }


  public void testAlivePredicate () {
    LispObject lispObject = evaluateString("(buffer-live-p (get-buffer \"1.txt\"))");
    Assert.assertEquals(LispSymbol.T, lispObject);
    lispObject = evaluateString("(kill-buffer \"1.txt\")");
    Assert.assertEquals(LispSymbol.T, lispObject);
    lispObject = evaluateString("(buffer-live-p (get-buffer \"1.txt\"))");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
    lispObject = evaluateString("(buffer-live-p \"1.txt\")");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
  }

  public void testReplaceBufferInWindows () {
    LispObject lispObject = evaluateString("(replace-buffer-in-windows)");
    Assert.assertEquals(LispSymbol.NIL, lispObject);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), myTests.get(myTestFiles[1]));
  }

  public void testKillLastBuffer () {
    Assert.assertEquals(myEnvironment.getBuffersSize(), 6);
    LispObject lispObject = evaluateString("(kill-buffer \"5.txt\")");
    Assert.assertEquals(LispSymbol.T, lispObject);
    lispObject = myEnvironment.getBufferCurrentForEditing();
    Assert.assertEquals(myEnvironment.findBufferSafe(myTestFiles[1]), lispObject);
    Assert.assertEquals(myEnvironment.getBuffersSize(), 5);
    Assert.assertNull(myEnvironment.findBuffer("5.txt"));
  }

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

  public void testBufferEndWrongArg () {
    try {
      evaluateString("(buffer-end 'a)");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument number-or-marker-p a)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testDefaultValueGlobals() {
    LispObject r = evaluateString("(default-value 'default-directory)");
    Assert.assertEquals(LispSymbol.NIL, r);
    r = evaluateString("(default-value 'mark-active)");
    Assert.assertEquals(LispSymbol.NIL, r);
  }

  //todo simple.el required
//    public void testDefaultValueExtern() {
//        LispObject r = evaluateString("(default-value 'mark-ring)");
//        Assert.assertEquals(LispSymbol.NIL, r);
//    }

  public void testDefaultValueBufferLocals() {
    try {
      evaluateString("(default-value 'is-alive)");
    } catch (Exception e) {
      Assert.assertEquals("'(void-variable is-alive)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }


  public void testInsertWrongType() {
    try {
      evaluateString("(insert 1 2 1.2)");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument char-or-string-p 1.2)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testInsertInts() {
    evaluateString("(push-mark 3 t t)");
    LispBuffer buffer = myEnvironment.getBufferCurrentForEditing();
    String insertion = "^A^B^C";
    String text = buffer.getText();
    Assert.assertFalse(text.contains(insertion));
    int point = buffer.point();
    LispMarker mark = buffer.getMark();
    Assert.assertEquals("3", mark.getPosition().toString());
    LispObject result = evaluateString("(insert 1 2 3)");
    Assert.assertEquals(LispSymbol.NIL, result);
    text = buffer.getText();
    System.out.println(text);
    Assert.assertTrue(text.contains(insertion));
    Assert.assertEquals(point + insertion.length(), buffer.point());
    Assert.assertEquals(3 + insertion.length(), (int)buffer.getMark().getPosition());
  }

  public void testGetBufferWindows() {
    LispObject windows = evaluateString("(get-buffer-window-list)");
    System.out.println(windows.toString());
  }

  public void testInternalCompleteBuffer () {
    LispObject completion = evaluateString("(internal-complete-buffer \"1\" nil t)");
    Assert.assertEquals(LispList.list(new LispString("1.txt")), completion);
    completion = evaluateString("(internal-complete-buffer \"1\" nil nil)");
    Assert.assertEquals(new LispString("1.txt"), completion);
    completion = evaluateString("(internal-complete-buffer \"1\" nil 5)");
    Assert.assertEquals(LispSymbol.T, completion); //todo: emacs shows nil
  }

  public void testAddTextPropertyOutOfRange() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    try {
      evaluateString("(add-text-properties 0 2 '(a b))");
    } catch (Exception e) {
      Assert.assertEquals("'(args-out-of-range 0 2)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testAddTextPropertyOk() {
    evaluateString("(switch-to-buffer \"3.txt\")");
    Assert.assertEquals(LispSymbol.T, evaluateString("(add-text-properties 1 3 '(a b))"));
    LispObject substring = evaluateString("(buffer-substring 2 3)");
    LispString expected = new LispString("a");
    expected.getTextPropertiesHolder().actOnTextProperties(0, 1, LispList.list(new LispSymbol("a"), new LispSymbol("b")), Action.ADD);
    Assert.assertEquals(expected, substring);
  }

  public void testSubstring () {
    evaluateString("(switch-to-buffer \"3.txt\")");
    LispObject substring = evaluateString("(buffer-substring 1 3)");
    Assert.assertEquals(new LispString("la"), substring);
    substring = evaluateString("(buffer-substring-no-properties 1 2)");
    Assert.assertEquals(new LispString("l"), substring);
  }

  public void testMakeVarBufferLocal() {
    Assert.assertEquals(new LispSymbol("a"), evaluateString("(make-variable-buffer-local 'a)"));
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(local-variable-p 'a)"));
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-if-set-p 'a)"));

    LispBuffer current = (LispBuffer) evaluateString("(current-buffer)");
    SpecialForms.setq(current.getEnvironment(), new LispSymbol("a"), new LispInteger(5));
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-p 'a)"));
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(local-variable-if-set-p 'a)"));

    Assert.assertEquals(new LispInteger(5), evaluateString("(buffer-local-value 'a (current-buffer))"));
  }

  public void testBufferLocalityVoid() {
    try {
      evaluateString("(buffer-local-value 'b (current-buffer))");
    } catch (Exception e) {
      Assert.assertEquals("'(void-variable b)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testBufferLocalityNotLocal() {
    LispBuffer current = (LispBuffer) evaluateString("(current-buffer)");
    SpecialForms.setq(current.getEnvironment(), new LispSymbol("a"), new LispInteger(1));
    Assert.assertEquals(new LispInteger(1), evaluateString("(buffer-local-value 'a (current-buffer))"));
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(local-variable-p 'a)"));
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(local-variable-if-set-p 'a)"));
  }

  public void testBufferLocalityLocal() {
    LispBuffer current = (LispBuffer) evaluateString("(current-buffer)");
    Environment e = current.getEnvironment();
    SpecialForms.setq(e, new LispSymbol("a"), new LispInteger(1));
    LispSymbol var = (LispSymbol) evaluateString("(make-local-variable 'a)");
    Core.set(e, var, new LispInteger(5));
    LispBuffer other = (LispBuffer) evaluateString("(other-buffer)");
    Assert.assertNotSame(current, other);
    String otherName = other.getName();
    Assert.assertEquals(new LispInteger(5), evaluateString("(buffer-local-value 'a (current-buffer))"));
    Assert.assertEquals(new LispInteger(1), evaluateString("(buffer-local-value 'a (get-buffer \"" + otherName + "\"))"));
  }

  public void testBufferLocalityAlias() {
    LispBuffer current = (LispBuffer) evaluateString("(current-buffer)");
    SpecialForms.setq(current.getEnvironment(), new LispSymbol("q"), new LispInteger(1));
    Core.defineVariableAlias(current.getEnvironment(), new LispSymbol("w"), new LispSymbol("q"), null);
    Assert.assertEquals(new LispSymbol("q"), evaluateString("(make-local-variable 'w)"));
    Assert.assertEquals(new LispInteger(1), evaluateString("(buffer-local-value 'w (current-buffer))"));
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-p 'w)"));
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-p 'q)"));

    LispBuffer other = (LispBuffer) evaluateString("(other-buffer)");
    Assert.assertNotSame(current, other);
    String otherName = other.getName();
    Assert.assertEquals(new LispInteger(1), evaluateString("(buffer-local-value 'w (get-buffer \"" + otherName + "\"))"));
    Assert.assertEquals(new LispInteger(1), evaluateString("(buffer-local-value 'q (get-buffer \"" + otherName + "\"))"));
  }

  public void testLocalityAlias2() {
    evaluateString("(setq q 1)");
    evaluateString("(defvaralias 'w 'q)");
    evaluateString("(make-local-variable 'q)");
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-p 'q)"));
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-p 'w)"));
    LispBuffer current = (LispBuffer) evaluateString("(current-buffer)");
    Core.defineVariableAlias(current.getEnvironment(), new LispSymbol("a"), new LispSymbol("w"), null);
    Assert.assertEquals(LispSymbol.T, evaluateString("(local-variable-p 'a)"));
  }

  public void testKillVar() {
    LispBuffer current = (LispBuffer) evaluateString("(current-buffer)");
    Environment e = current.getEnvironment();
    Core.set(e, new LispSymbol("a"), new LispInteger(1));
    LispSymbol var = (LispSymbol) evaluateString("(make-local-variable 'a)");
    Core.set(e, var, new LispInteger(5));

//        evaluateString("(set (make-local-variable 'a) 5)");
    Map<LispSymbol, LispObject> varMap = current.getAllLocalVarValues();
    Assert.assertTrue(varMap.containsKey(new LispSymbol("a")));
    Assert.assertEquals(new LispInteger(5), varMap.get(new LispSymbol("a")));

    Assert.assertEquals(new LispSymbol("a"), evaluateString("(kill-local-variable 'a)"));
    varMap = current.getAllLocalVarValues();
    Assert.assertFalse(varMap.containsKey(new LispSymbol("a")));
  }

  public void testLookingAt() {
    String regexp = callLookingAt(4, LispSymbol.T);
    Assert.assertEquals(myEnvironment.getBufferCurrentForEditing(), Match.getBuffer());
    Assert.assertArrayEquals(new Integer[] {4, 4 + regexp.length()}, Match.getLastMatch().toArray(new Integer[2]));
  }

  public void testLookingAtFurther() {
    callLookingAt(2, LispSymbol.NIL);
    Assert.assertNull(Match.getBuffer());
    Assert.assertTrue(Match.getLastMatch().isEmpty());
  }

  public void testLookingAtFalse() {
    callLookingAt(5, LispSymbol.NIL);
    Assert.assertNull(Match.getBuffer());
    Assert.assertTrue(Match.getLastMatch().isEmpty());
  }

  private String callLookingAt(int point, LispSymbol expectedValue) {
    evaluateString("(switch-to-buffer \"5.txt\")");
    evaluateString("(goto-char " + point + ")");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(match-data)"));
    LispBuffer buffer = (LispBuffer) Buffer.getBuffer(myEnvironment, new LispString("5.txt"));
    Assert.assertEquals(buffer, myEnvironment.getBufferCurrentForEditing());
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(match-data)"));
    String regexp = "sage";
    Assert.assertEquals(expectedValue, evaluateString("(looking-at \"" + regexp + "\")"));
    return regexp;
  }

  //requirements: load font-lock.el (set TestMode.LOAD_FILES = true)
  public void testAssignFaces() {
    evaluateString("(switch-to-buffer \"5.txt\")");
    LispObject maxPoint = evaluateString("(point-max)");
    Assert.assertEquals(LispSymbol.NIL, evaluateString("(font-lock-fontify-syntactically-region (point-min) (point-max) t)"));
    Assert.assertEquals(maxPoint, evaluateString("(point)"));
  }

  public void testToggleOnFontLockMode() {
    assertT(evaluateString("(font-lock-mode t)"));
  }

  public void testFontLockVerbose() {
    Assert.assertEquals(new LispInteger(0), evaluateString("font-lock-verbose"));
  }

  public void testFontifyBuffer() {
    LispObject q = evaluateString("(font-lock-fontify-buffer)");
    System.out.println(q);
  }

  public void testBOL() {
    assertNil(evaluateString("(beginning-of-line -100)"));
    LispBuffer buffer = myEnvironment.getBufferCurrentForEditing();
    assertEquals(1, buffer.getLine());
    assertEquals(buffer.getLineStartIndex(), buffer.getColumn());
    assertEquals(1, buffer.point());
  }

  /* todo get back
  public void testSetLispMode() {
      evaluateString("(emacs-lisp-mode)");
      LispObject cmd = evaluateString("(key-binding \"\\C-x\\C-e\")");
      LispObject globalMap = evaluateString("(current-global-map)");
      LispObject localMap = evaluateString("(current-local-map)");
      Assert.assertEquals(new LispSymbol("emacs-lisp-mode"), evaluateString("major-mode"));

      System.out.println(cmd);
  }

  public void testSetFontLockMinorMode() {
      DefinitionLoader.loadFile("emacs-lisp/lisp-mode.el");
      evaluateString("(emacs-lisp-mode)");
      evaluateString("(font-lock-mode)");
      LispSymbol f = (LispSymbol) evaluateString("'font-lock-string-face");
      Assert.assertTrue(f == f.getValue());
      LispObject fProp = evaluateString("(symbol-plist 'font-lock-string-face)");
      LispObject fValueProp = evaluateString("(symbol-plist (symbol-value 'font-lock-string-face))");
      Assert.assertEquals(fProp, fValueProp);
//        System.out.println(fValueProp.toString());

      evaluateString("(switch-to-buffer \"5.txt\")");
      Assert.assertEquals(f, evaluateString("(get-char-property 5 'face)"));
  }
  */

  /*
  //todo: load (void-function timer-create)
  public void testSetf() {
      evaluateString("(setq timer (timer-create))");
      Assert.assertEquals(new LispInteger(4), evaluateString("(setf (timer--function timer) 4)"));
      Assert.assertEquals(new LispVector(LispSymbol.T, LispSymbol.NIL, LispSymbol.NIL, LispSymbol.NIL,
              LispSymbol.NIL, new LispInteger(4), LispSymbol.NIL, LispSymbol.NIL), evaluateString("timer"));
  }

  //todo load (void macro timer--function)
  public void testClSetfDoModifyAndSet() {
      evaluateString("(setq args '((timer--function timer) function))");
      LispObject method = evaluateString("'(nil (--cl-store-- progn (aset timer 5 --cl-store--)) (timer--function timer))");
      evaluateString("(setq method (cl-setf-do-modify (car args) (nth 1 args)))");
      Assert.assertEquals(method, evaluateString("method"));
      LispObject store = evaluateString("'(progn (aset timer 5 function))");
      Assert.assertEquals(store, evaluateString("(cl-setf-do-store (nth 1 method) (nth 1 args))"));
  }
  */
}


