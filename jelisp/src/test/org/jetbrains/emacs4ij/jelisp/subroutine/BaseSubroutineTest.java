package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestSetup;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Before;
import org.junit.BeforeClass;

public abstract class BaseSubroutineTest {
  protected Environment myEnvironment;

  @BeforeClass
  public static void runBeforeClass() {
    TestSetup.runBeforeClass();
  }

  @Before
  public void setUp() throws Exception {
    GlobalEnvironment.INSTANCE.clearRecorded();
    myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
  }

  protected LispObject evaluateString (String lispCode) throws LispException {
    ForwardParser forwardParser = new ForwardParser();
    LispObject object = forwardParser.parseLine(lispCode);
    return object.evaluate(myEnvironment);
  }

}
