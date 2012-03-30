package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 4:31 PM
 * To change this template use File | Settings | File Templates.
 */
public interface StringOrVector extends LispObject { //} LispCommand {
    List<Shortcut> toKeyboardShortcutList();
    int length();
}
