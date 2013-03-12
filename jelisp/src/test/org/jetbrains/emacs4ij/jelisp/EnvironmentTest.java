package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class EnvironmentTest extends JelispTestCase {
  @Test
  public void testGetBuiltInF () {
    LispObject lispObject = myEnvironment.find("+");
    Assert.assertTrue(lispObject instanceof LispSymbol);
  }

  @Test
  public void testOverrideVar () {
    myEnvironment.defineSymbol(new LispSymbol("a", new LispInteger(5)));
    myEnvironment.defineSymbol(new LispSymbol("a", new LispInteger(6)));
    Assert.assertEquals(new LispInteger(6), (myEnvironment.find("a")).getValue());
  }

  @Test
  public void testCommandList() {
    List<String> commandList = GlobalEnvironment.INSTANCE.getCommandList("f");
    Assert.assertFalse(commandList.isEmpty());
  }
}
