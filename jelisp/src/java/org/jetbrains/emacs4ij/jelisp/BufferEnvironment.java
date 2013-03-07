package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;

import java.util.Map;

public final class BufferEnvironment extends Environment {
  public BufferEnvironment(@NotNull final Environment outerEnv) {
    myOuterEnv = outerEnv;
  }

  public void getVariableEntries (Map<LispSymbol, LispObject> map){
    for (LispSymbol symbol: mySymbols.values()) {
      map.put(new LispSymbol(symbol.getName()), symbol.getValue());
    }
  }

  @NotNull
  public LispBuffer getCurrentNonToolBuffer() {
    LispBuffer nonToolBuffer = ourBufferManager.getCurrentNonToolBuffer();
    if (nonToolBuffer == null)
      throw new NoOpenedBufferException();
    return nonToolBuffer;
  }
}
