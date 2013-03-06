package org.jetbrains.emacs4ij.jelisp;

import com.intellij.openapi.diagnostic.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Calendar;

public final class LogUtil {
  private static SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
  private static final Logger LOG = Logger.getInstance("#org.jetbrains.emacs4ij.jelisp.LogUtil");

  public static void info(String msg) {
    if (TestMode.isLoggingEnabled()) {
      LOG.info(msg);
    } else {
      printToConsole(msg, System.out);
    }
  }

  public static void info(Logger logger, String msg) {
    if (TestMode.isLoggingEnabled()) {
      logger.info(msg);
    } else {
      printToConsole(msg, System.out);
    }
  }

  public static void log(String msg, GlobalEnvironment.MessageType type) {
    log(LOG, msg, type);
  }

  public static void log(Throwable t) {
    LOG.error(t);
  }

  public static void log(String msg, Throwable t) {
    LOG.error(msg, t);
  }

  public static void log(Logger logger, String msg, GlobalEnvironment.MessageType type) {
    if (TestMode.isLoggingEnabled()) {
      switch (type) {
        case WARNING:
          logger.warn(msg);
          break;
        case ERROR:
          logger.error(msg);
          break;
        default:
          logger.info(msg);
      }
    } else {
      boolean isErr = type == GlobalEnvironment.MessageType.ERROR || type == GlobalEnvironment.MessageType.WARNING;
      printToConsole(msg, isErr ? System.err : System.out);
    }
  }

  private static void printToConsole(String msg, @NotNull PrintStream out) {
    out.println(sdf.format(Calendar.getInstance().getTime()) + " " + msg);
  }
}
