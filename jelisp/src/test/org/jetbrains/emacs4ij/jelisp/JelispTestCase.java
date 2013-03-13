package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;
import org.jetbrains.emacs4ij.jelisp.parser.ForwardParser;
import org.junit.Before;
import org.junit.BeforeClass;

public class JelispTestCase {
  protected CustomEnvironment myEnvironment;

  @BeforeClass
  public static void runBeforeClass() {
    TestMode.TEST = true;
//    TestMode.EXTRACT_DOC = false;
//    TestMode.LOAD_FILES = false;
//    TestMode.INIT_GLOBAL_ENV_FROM_EMACS_SOURCES = false;

    DefinitionLoader.addSkipForms("(eval-when-compile ", "(defvar special-mode-map");

    if (GlobalEnvironment.INSTANCE == null) {
      LogUtil.info("INIT GLOBAL ENV");
      GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
      GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.4");
      DefinitionLoader.initialize(new DefinitionIndex());
      GlobalEnvironment.initialize(null, null, null, null, new Runnable() {
        @Override
        public void run() {
          new TestMinibuffer();
        }
      });
    }
    GlobalEnvironment.INSTANCE.startRecording();

    try {
      GlobalEnvironment.INSTANCE.getMinibuffer();
    } catch (UnregisteredBufferException e) {
      LogUtil.info("recreate mini buffer");
      new TestMinibuffer();
    }
  }

  @Before
  public void setUp() throws Exception {
    GlobalEnvironment.INSTANCE.clearRecorded();
    myEnvironment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
  }

  public static String getCauseMsg(Throwable e) {
    if (e.getCause() == null) return e.getMessage();
    return getCauseMsg(e.getCause());
  }

  protected LispObject evaluateString (String lispCode) throws LispException {
    return new ForwardParser().parseLine(lispCode).evaluate(myEnvironment);
  }
}
