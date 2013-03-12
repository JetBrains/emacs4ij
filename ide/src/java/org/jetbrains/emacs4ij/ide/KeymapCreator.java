package org.jetbrains.emacs4ij.ide;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymapFactory;

public class KeymapCreator implements LispKeymapFactory {
  @Override
  public LispKeymap createKeymap(@Nullable LispObject name, @Nullable LispKeymap parent) {
    return new IdeaKeymap(name, parent);
  }
}
