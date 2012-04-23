package org.jetbrains.emacs4ij.jelisp.interactive;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/18/12
 * Time: 10:22 AM
 * To change this template use File | Settings | File Templates.
 */

import java.util.List;

/**
 * temporary interface while i didn't provide minibuffer-complete work
 */
public interface Completer {
    List<String> getCompletions (String parameter);
    void setNoMatch (String parameter);

}
