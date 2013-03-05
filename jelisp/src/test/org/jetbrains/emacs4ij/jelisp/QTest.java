package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.junit.Test;

import java.text.SimpleDateFormat;
import java.util.Date;

public class QTest {
  @Test
  public void q() {
    System.out.println(new SimpleDateFormat().format(new Date(System.currentTimeMillis())));
    System.out.println(new SimpleDateFormat("dd.MM.yyyy HH:mm:ss").format(new Date(System.currentTimeMillis())));
    System.out.println(new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date(System.currentTimeMillis())));

    System.out.println(LispInteger.MAX_CHAR);
  }
}
