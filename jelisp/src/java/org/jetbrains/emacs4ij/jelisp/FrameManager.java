package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;

import java.util.List;

public interface FrameManager extends Manager<LispFrame> {
  void onFrameOpened (LispFrame frame);
  void onFrameReleased (LispFrame frame);
  List<LispFrame> getVisibleFrames();
  List<LispFrame> getVisibleAndIconifiedFrames();
  @NotNull LispFrame getExistingFrame (LispFrame frame);
}
