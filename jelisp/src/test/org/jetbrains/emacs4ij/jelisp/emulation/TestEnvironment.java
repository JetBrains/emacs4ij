package org.jetbrains.emacs4ij.jelisp.emulation;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;

public class TestEnvironment extends Environment {
    public TestEnvironment(@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;

        //todo set TestMinibuffer
    }
}
