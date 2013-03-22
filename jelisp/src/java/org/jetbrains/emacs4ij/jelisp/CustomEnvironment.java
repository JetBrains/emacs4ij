package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;

public final class CustomEnvironment extends Environment {
  public CustomEnvironment(@NotNull final Environment outerEnv) {
    myOuterEnv = outerEnv;
  }
}
