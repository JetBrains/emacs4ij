package org.jetbrains.emacs4ij.jelisp.platformDependent;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.LogUtil;
import org.jetbrains.emacs4ij.jelisp.TestMode;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.Emacs4ijFatalException;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.interactive.InteractiveReader;
import org.jetbrains.emacs4ij.jelisp.subroutine.Minibuffer;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;

public abstract class LispMinibuffer extends LispBuffer {
  private LispBuffer myParent;
  private InteractiveReader myInteractive;
  private int myActivationsDepth = 0;
  private boolean isOpened = false;
  private boolean exitOnSuccess = false;
  
  private static Deque<InteractiveReader> myInteractiveStack = new ArrayDeque<>();

  protected LispMinibuffer(String name, Environment environment, LispBuffer parent) {
    super(name, environment);
    myParent = parent;
  }

  public abstract void setCharListener();

  protected abstract void clearNoMatch();
  protected abstract Integer getCharCode();
  protected abstract void resetCharCode();
  protected abstract void cancelNoMatchMessageUpdate();
  protected abstract void write(final String text);

  public void updateEditorText() {
    String text = myInteractive.getPrompt() +
        ((myInteractive.getInitialInput() == null) ? "" : myInteractive.getInitialInput());
    write(text);
  }

  @Override
  public void kill() {
    exitOnSuccess = false;
    cancelNoMatchMessageUpdate();
    myActivationsDepth = 0;
    if (myParent != null) {
      myParent.closeHeader();
      myParent = null;
    }
    isOpened = false;
    LogUtil.info("kill minibuffer");
//  todo: I should do this way, probably. So when 1 minibuffer is closed, the previous one is recovered
//    myActivationsDepth--;
  }

  protected void open(@Nullable LispBuffer parent) {
    if (parent != null && parent.isToolBuffer())
      throw new Emacs4ijFatalException(JelispBundle.message("open.minibuffer.in.tool.window"));

    myParent = parent;
    myActivationsDepth++;
    isOpened = true;

    if (myActivationsDepth > 1) {
      getEnvironment().killBuffer(this);
      GlobalEnvironment.echo(JelispBundle.message("call.interactively.message"), GlobalEnvironment.MessageType.WARNING);
    }
  }

  /*
  ** for usage from SpecialFormInteractive only
  */
  public void readParameter() {
    if (!isOpened) {
      try {
        open(getEnvironment().getCurrentNonToolBuffer());
      } catch (NoOpenedBufferException e) {
        if (TestMode.TEST) {
          open(null);
        } else {
          throw e;
        }
      }
    }
    if (myInteractive.toShowSpecialNoMatchMessage()) {
      write(myInteractive.getNoMatchMessage());
      clearNoMatch();
      return;
    }
    String text = myInteractive.getPrompt() + (myInteractive.getInitialInput() == null ? "" : myInteractive.getInitialInput());
    int cursorPosition = text.length() + 1;
    text += myInteractive.getNoMatchMessage();
    write(text);

    gotoChar(cursorPosition);
    //todo unchangeable prompt
    setActive();

    clearNoMatch();
  }

  public int getActivationsDepth() {
    return myActivationsDepth;
  }

  public void setInputStartValue (String startValue) {
    myInteractive.setInitialInput(startValue);
  }

  public void onReadInput () {
    myInteractive.onReadParameter(readInputString());
    if (myInteractive.isFinished()) {
      runInteractive();
    } else if (!myInteractive.isNoMatch())
      myInteractive.readNextArgument();
  }

  public void onInteractiveNoIoInput(InteractiveReader interactive) {
    if (new LispSymbol("exit-minibuffer").equals(interactive.getCommand())) {
      exitOnSuccess = true;
      onReadInput();
      return;
    }
    if (myInteractive != interactive) {
      if (myInteractive != null && !myInteractive.isFinished()) {
        myInteractiveStack.push(myInteractive);
      }
      myInteractive = interactive;
    }
    if (myInteractive.isFinished()) {
      runInteractive();
      return;
    }
    myInteractive.readNextArgument();
  }

  //todo: as in emacs
  public String readInputString() {
    if (getCharCode() != null) {
      String code = getCharCode().toString();
      resetCharCode();
      return code;
    }
    String text = getText();
    int k = myInteractive.isNoMatch()
        ? text.lastIndexOf(myInteractive.getNoMatchMessage())
        : text.length();
    if (k < 0)
      k = text.length();
    if (myInteractive.getPrompt() == null)
      System.out.print(1);
    return text.substring(myInteractive.getPrompt().length(), k);
  }

  //it's for completer interface
  public List<String> getCompletions (String parameter) {
    return myInteractive.getCompletions(parameter);
  }

  public void setNoMatch(String input) {
    myInteractive.setNoMatch(input);
  }

  //is public only for test!
  public LispSymbol returnDefault(LispObject defaultValue) {
    if (defaultValue == null)
      return new LispSymbol("");
    if (defaultValue instanceof LispSymbol) {
      if (!defaultValue.equals(LispSymbol.ourNil))
        return (LispSymbol) defaultValue;
      return new LispSymbol("");
    }
    if (defaultValue instanceof LispString) {
      String name = ((LispString) defaultValue).getData();
      LispSymbol ret = getEnvironment().find(name);
      if (ret == null) {
        return new LispSymbol(name);
      }
      return ret;
    }
    if (defaultValue instanceof LispList) {
      defaultValue = ((LispList) defaultValue).car();
      if (!(defaultValue instanceof LispList))
        return returnDefault(defaultValue);
    }
    throw new WrongTypeArgumentException("stringp", defaultValue.toString());
  }

  private void runInteractive() {
    if (isOpened && exitOnSuccess) {
      getEnvironment().killBuffer(this);
    }
    InteractiveReader current = myInteractive;
    myInteractive = !myInteractiveStack.isEmpty() ? myInteractiveStack.removeFirst() : null;
    Minibuffer.goOn(current);
  }

  protected InteractiveReader getInteractive() {
    return myInteractive;
  }
}
