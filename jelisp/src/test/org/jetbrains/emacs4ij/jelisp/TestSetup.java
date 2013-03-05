package org.jetbrains.emacs4ij.jelisp;

public abstract class TestSetup {
  public static void runBeforeClass() {
    TestMode.TEST = true;
    TestMode.LOAD_FILES = false;
    TestMode.EXTRACT_DOC = false;
    try {
      if (GlobalEnvironment.INSTANCE == null) {
        LogUtil.info("INIT GLOBAL ENV");
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
        GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.4");
        DefinitionLoader.initialize(new DefinitionIndex());
        GlobalEnvironment.initialize(null, null, null, null);
      }
      GlobalEnvironment.INSTANCE.startRecording();
    } catch (RuntimeException e) {
      e.printStackTrace();
    }
  }

  public static String getCause (Throwable e) {
    if (e.getCause() == null) return e.getMessage();
    return getCause(e.getCause());
  }
}
