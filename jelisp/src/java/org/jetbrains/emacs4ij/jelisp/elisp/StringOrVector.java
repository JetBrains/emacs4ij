package org.jetbrains.emacs4ij.jelisp.elisp;

import com.intellij.openapi.actionSystem.Shortcut;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 4:31 PM
 * To change this template use File | Settings | File Templates.
 */
public interface StringOrVector extends LispObject {
    @Nullable
    List<Shortcut> toKeyboardShortcutList();
    int size();
    StringOrVector substring (int from, int to);
}
