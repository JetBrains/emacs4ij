package org.jetbrains.emacs4ij.jelisp.platformDependent;

import org.jetbrains.emacs4ij.jelisp.interactive.InteractiveReader;

import java.util.List;

public interface LispMinibuffer extends LispBuffer {
  void readParameter(InteractiveReader interactive);
  void addCharListener();
  int getActivationsDepth();
  void setInputStartValue (String startValue);
  void updateEditorText();

  void onReadInput();
  void onInteractiveNoIoInput(InteractiveReader interactive);

  //todo: as in emacs
  String readInputString();

  //it's for completer interface
  List<String> getCompletions (String parameter);
  void setNoMatch(String input);
}
