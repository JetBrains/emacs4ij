package org.jetbrains.emacs4ij.ide;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.junit.Assert;
import org.junit.Before;

public class ScanTest extends IdeTestCase {
  @Before
  public void setUp() throws Exception {
    super.setUp();
    setTestFiles(false);
  }

  public void testEvalLastSexp() {
    Assert.assertEquals(myEnvironment.findBufferSafe("3.txt"), evaluateString("(switch-to-buffer \"3.txt\")"));
    LispBuffer buffer = myEnvironment.getBufferCurrentForEditing();
    buffer.gotoChar(buffer.pointMax());
    LispObject result = evaluateString("(eval-last-sexp nil)");
    Assert.assertEquals(new LispInteger(8), result);
  }

  public void testScanSexpsForward() {
    LispBuffer buffer1 = myEnvironment.findBufferSafe("4.txt");
    myEnvironment.switchToBuffer(buffer1);
    myEnvironment.setBufferCurrentForEditing(buffer1);
    LispObject sexpEnd = evaluateString("(scan-sexps 1 1)");
    Assert.assertEquals(new LispInteger(7), sexpEnd);
  }

  public void testScanSexpsForwardString() {
    LispBuffer buffer1 = myEnvironment.findBufferSafe("4.txt");
    myEnvironment.switchToBuffer(buffer1);
    myEnvironment.setBufferCurrentForEditing(buffer1);
    LispObject sexpEnd = evaluateString("(scan-sexps 30 1)");
    Assert.assertEquals(new LispInteger(38), sexpEnd);
  }

  public void testScanSexpsForwardStringWithEscaped() {
    LispBuffer buffer1 = myEnvironment.findBufferSafe("4.txt");
    myEnvironment.switchToBuffer(buffer1);
    myEnvironment.setBufferCurrentForEditing(buffer1);
    LispObject sexpEnd = evaluateString("(scan-sexps 47 1)");
    Assert.assertEquals(new LispInteger(70), sexpEnd);
  }
}
