package org.jetbrains.emacs4ij.jelisp.interactive;

import java.util.List;

/**
 * temporary interface while i didn't provide minibuffer-complete work
 */
interface Completer {
  List<String> getCompletions (String parameter);
  void setNoMatch (String parameter);

}
