package org.jetbrains.emacs4ij.ide;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;
import org.junit.Assert;
import org.junit.Before;

public class MinibufferTest extends IdeTestCase {
  private LispMinibuffer myMiniBuffer;

  @Before
  public void setUp() throws Exception {
    super.setUp();
    myMiniBuffer = myEnvironment.getMinibuffer();
  }

  public void testReturnDefault_Integer () {
    try {
      myMiniBuffer.returnDefault(new LispInteger(5));
    } catch (WrongTypeArgumentException e) {
      Assert.assertTrue(true);
      return;
    }
    Assert.assertTrue(false);
  }

  public void testReturnDefault_StringList () {
    LispObject ret = myMiniBuffer.returnDefault (LispList.list(new LispString("hi"), new LispInteger(5)));
    Assert.assertEquals(new LispSymbol("hi"), ret);
  }

  public void testReturnDefault_String () {
    LispObject ret = myMiniBuffer.returnDefault (new LispString("hi"));
    Assert.assertEquals(new LispSymbol("hi"), ret);
  }

  public void testReturnDefault_IntList () {
    try {
      myMiniBuffer.returnDefault (LispList.list(new LispInteger(5), new LispString("hi")));
    } catch (WrongTypeArgumentException e) {
      Assert.assertTrue(true);
      return;
    }
    Assert.assertTrue(false);
  }

  public void testReturnDefault_ListList () {
    try {
      myMiniBuffer.returnDefault (LispList.list(LispList.list(new LispString("wow"), new LispString("hi")), new LispInteger(5), new LispString("hi")));
    } catch (WrongTypeArgumentException e) {
      Assert.assertTrue(true);
      return;
    }
    Assert.assertTrue(false);
  }

  public void testReturnDefault_ExistingSymbol () {
    evaluateString("(setq s 5)");
    LispObject ret = myMiniBuffer.returnDefault (new LispSymbol("s"));
    Assert.assertEquals(new LispSymbol("s", new LispInteger(5)), ret);
  }

  public void testReturnDefault_NonExistingSymbol () {
    LispObject ret = myMiniBuffer.returnDefault (new LispSymbol("s"));
    Assert.assertEquals(new LispSymbol("s"), ret);
  }

  public void testReturnDefault_NilSymbol () {
    LispObject ret = myMiniBuffer.returnDefault (LispSymbol.NIL);
    Assert.assertEquals(new LispSymbol(""), ret);
  }

  public void testReturnDefault_EmptyList () {
    LispObject ret = myMiniBuffer.returnDefault (LispList.list());
    Assert.assertEquals(new LispSymbol(""), ret);
  }

//    //todo '(void-function make-overlay)
//    public void testMessage() {
//        evaluateString("(minibuffer-message \"test\")");
//    }
}
