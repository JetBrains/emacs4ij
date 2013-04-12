package org.jetbrains.emacs4ij.jelisp.interactive;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LambdaOrSymbolWithFunction;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;

import java.util.List;

public final class EmptyReader extends InteractiveReader {
  public EmptyReader (Environment environment, LambdaOrSymbolWithFunction command, LispList args) {
    super(environment, command);
    myArguments = args.toLispObjectList();
  }

  @Override
  public boolean isFinished() {
    return true;
  }

  @Override
  public boolean isNoMatch() {
    return false;
  }

  @Override
  public String getNoMatchMessage() {
    return ourEmptyMessage;
  }

  @Override
  public boolean toShowSpecialNoMatchMessage() {
    return false;
  }

  @Override
  public void readNextArgument() {
    throw new UnsupportedOperationException("EmptyReader.readNextArgument");
  }

  @Override
  public List<String> getCompletions(String parameter) {
    throw new UnsupportedOperationException("EmptyReader.getCompletions");
  }

  @Override
  public void setNoMatch(String parameter) {
    throw new UnsupportedOperationException("EmptyReader.setNoMatch");
  }

  @Override
  public void onReadParameter(String parameter) {
    throw new UnsupportedOperationException("EmptyReader.onReadParameter");
  }
}
