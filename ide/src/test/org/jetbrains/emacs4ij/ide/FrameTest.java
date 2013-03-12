package org.jetbrains.emacs4ij.ide;

import junit.framework.Assert;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Before;

public class FrameTest extends IdeTestCase {
  @Before
  public void setUp() throws Exception {
    super.setUp();
    setTestFiles(false);
  }

  public void testFrameParameters() {
    Assert.assertEquals(LispSymbol.ourNil, evaluateString("(modify-frame-parameters nil '((test 2 3) (a 1) . 5))"));
    Assert.assertEquals(LispList.list(new LispInteger(2), new LispInteger(3)), evaluateString("(frame-parameter nil 'test)"));
    Assert.assertEquals(LispList.list(new LispInteger(1)), evaluateString("(frame-parameter nil 'a)"));
    LispObject parameters = evaluateString("(frame-parameters)");
    Assert.assertTrue(parameters.toString().contains("(test 2 3)") && parameters.toString().contains("(a 1)"));
    Assert.assertEquals(LispSymbol.ourNil, evaluateString("(frame-parameter nil 'buffer-predicate)"));
  }

  public void testFrameParametersWrongArg() {
    try {
      evaluateString("(modify-frame-parameters nil '(test 1))");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument listp test)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }

  public void testFrameParametersWrongName() {
    try {
      evaluateString("(modify-frame-parameters nil '((2 1)))");
    } catch (Exception e) {
      Assert.assertEquals("'(wrong-type-argument symbolp 2)", getCauseMsg(e));
      return;
    }
    Assert.fail();
  }
}