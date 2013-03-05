package org.jetbrains.emacs4ij.jelisp;

public class TestMode {
  public static boolean TEST = false;
  public static boolean EXTRACT_DOC = true;
  public static boolean LOAD_FILES = true;
  public static boolean INIT_GLOBAL_ENV_FROM_EMACS_SOURCES = true;
  public static boolean LOGGING_ENABLED = true;

  public static boolean isLoggingEnabled() {
    return !TEST && LOGGING_ENABLED;
  }
}
