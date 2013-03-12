package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredBufferException;

public abstract class TestSetup {
  public static void runBeforeClass() {
    TestMode.TEST = true;
    TestMode.LOAD_FILES = false;
    TestMode.EXTRACT_DOC = false;
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

  public static String getCause (Throwable e) {
    if (e.getCause() == null) return e.getMessage();
    return getCause(e.getCause());
  }
}
