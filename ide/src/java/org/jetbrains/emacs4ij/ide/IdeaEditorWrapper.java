package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.platformDependent.EditorWrapper;


public final class IdeaEditorWrapper implements EditorWrapper {
  private final Editor myEditor;

  public IdeaEditorWrapper (final Editor editor) {
    myEditor = editor;
  }

  public Editor getEditor() {
    return myEditor;
  }
}
