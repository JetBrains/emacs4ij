package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.DuplicateFrame;
import org.jetbrains.emacs4ij.jelisp.exception.NoLispFrameForIdeFrame;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedFrameException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredFrameException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

import java.util.ArrayList;
import java.util.List;


final class FrameManagerImpl extends CyclicManager<LispFrame> implements FrameManager {
  @Override
  public void onFrameOpened(LispFrame frame) {
    define(frame);
  }

  @Override
  public void onFrameReleased (LispFrame frame) {
    remove(frame);
  }

  @Override
  public List<LispFrame> getVisibleFrames () {
    List<LispFrame> visibleFrames = new ArrayList<>();
    for (LispFrame frame: myData) {
      if (Predicate.frameVisibleP(GlobalEnvironment.INSTANCE, frame).equals(LispSymbol.T))
        visibleFrames.add(frame);
    }
    return visibleFrames;
  }

  @Override
  public List<LispFrame> getVisibleAndIconifiedFrames () {
    List<LispFrame> frames = new ArrayList<>();
    for (LispFrame frame: myData) {
      LispSymbol predicate = Predicate.frameVisibleP(GlobalEnvironment.INSTANCE, frame);
      if (predicate.equals(LispSymbol.T) || predicate.equals(new LispSymbol("icon")))
        frames.add(frame);
    }
    return frames;
  }

  @NotNull
  @Override
  public LispFrame getExistingFrame(final LispFrame newFrame) {
    for (LispFrame frame: myData) {
      if (frame.equals(newFrame))
        return frame;
    }
    throw new NoLispFrameForIdeFrame();
  }

  @Override
  protected void throwNoOpenedItem() {
    throw new NoOpenedFrameException();
  }

  @Override
  protected void throwItemIsNotInDataSet(LispFrame item) {
    throw new UnregisteredFrameException(item.toString());
  }

  @Override
  protected void throwDuplicateItem(LispFrame item) {
    throw new DuplicateFrame(item.toString());
  }
}
