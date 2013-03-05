package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.exception.DoubleBufferException;
import org.jetbrains.emacs4ij.jelisp.exception.DuplicateWindow;
import org.jetbrains.emacs4ij.jelisp.exception.NoOpenedWindowException;
import org.jetbrains.emacs4ij.jelisp.exception.NoWindowForBuffer;
import org.jetbrains.emacs4ij.jelisp.exception.NoWindowForFrame;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredEditorException;
import org.jetbrains.emacs4ij.jelisp.exception.UnregisteredWindowException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.EditorWrapper;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispMinibuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispToolWindow;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindowFactory;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

final class WindowManager extends CyclicManager<LispWindow> {
  private final LispWindowFactory myFactory;

  public WindowManager (LispWindowFactory factory) {
    myFactory = factory;
  }

  public void onOpenToolBuffer (LispFrame frame, LispToolWindow window) {
    if (!window.isRegistered()) {
      int id = getFrameWindows(frame).size();
      window.setId(id);
      window.setFrame(frame);
      define(window);
    }
  }

  public void onOpenBuffer(LispBuffer buffer, LispFrame frame, EditorWrapper editor) {
    List<LispWindow> bufferWindowsOnFrame = getBufferWindowsOnFrame(buffer, frame);
    if (bufferWindowsOnFrame.isEmpty()) {
      int id = getFrameWindows(frame).size();
      LispWindow window = myFactory.createWindow(id, buffer, frame, editor);
      define(window);
      return;
    }
    if (bufferWindowsOnFrame.size() == 1 && !bufferWindowsOnFrame.get(0).isVisible()) {
      bufferWindowsOnFrame.get(0).open(editor);
      return;
    }
    throw new DoubleBufferException(buffer.getName());
  }

  @Override
  protected void throwNoOpenedItem() {
    throw new NoOpenedWindowException();
  }

  @Override
  protected void throwItemIsNotInDataSet(LispWindow item) {
    throw new UnregisteredWindowException(item.toString());
  }

  @Override
  protected void throwDuplicateItem(LispWindow item) {
    throw new DuplicateWindow(item);
  }

  public void onKillBuffer(LispBuffer buffer) {
    if (buffer instanceof LispMinibuffer)
      for (LispFrame frame: getBufferFrames(buffer)) {
        hideBufferOnFrame(buffer, frame);
      }
    else
      for (Iterator<LispWindow> iterator = myData.iterator(); iterator.hasNext(); ) {
        LispWindow window = iterator.next();
        if (window.getBuffer() == buffer)
          iterator.remove();
      }
  }

  @NotNull
  public LispWindow getBufferLastSelectedWindow (LispBuffer buffer) {
    for (LispWindow window: myData) {
      if (window.getBuffer().equals(buffer))
        return window;
    }
    throw new NoWindowForBuffer(buffer.getName());
  }

  /**
   *
   * @param frame on which must be found window
   * @param buffer which must be displayed by the found window
   * @return the first window displaying @param buffer on @param frame
   */
  @Nullable
  public LispWindow getBufferWindowOnFrame(LispFrame frame, LispBuffer buffer) {
    for (LispWindow window: myData) {
      if (window.getBuffer() == buffer && window.getFrame() == frame)
        return window;
    }
    return null;
  }

  private List<LispWindow> getBufferWindowsOnFrame (LispBuffer buffer, LispFrame frame) {
    List<LispWindow> windows = new ArrayList<>();
    for (int i =  myData.size() - 1; i > -1; i--) {
      LispWindow window = myData.get(i);
      if (window.getFrame().equals(frame) && window.getBuffer().equals(buffer))
        windows.add(window);
    }
    //verify visibility
    for (LispWindow window: windows) {
      if (!window.isVisible()) {
        assert windows.size() == 1;
        break;
      }
    }
    return windows;
  }

  @NotNull
  public LispWindow getFrameSelectedWindow (LispFrame frame) {
    for (LispWindow window: myData) {
      if (window.getFrame() == frame)
        return window;
    }
    throw new NoWindowForFrame(frame.toString());
  }

  public void deleteFrameOtherWindows (LispFrame frame, LispWindow window) {
    assert frame == window.getFrame();

    for (LispWindow w: myData) {
      if (!w.getFrame().equals(frame) || w.equals(window)) continue;
      w.close();
    }
  }

  public LispFrame[] getBufferFrames(LispBuffer buffer) {
    Set<LispFrame> frames = new LinkedHashSet<>();
    for (LispWindow window: myData) {
      if (window.getBuffer() == buffer)
        frames.add(window.getFrame());
    }
    return frames.toArray(new LispFrame[frames.size()]);
  }

  public LispBuffer[] getFrameBuffers(LispFrame frame) {
    Set<LispBuffer> buffers = new LinkedHashSet<>();
    for (LispWindow window: myData) {
      if (window.getFrame().equals(frame))
        buffers.add(window.getBuffer());
    }
    return buffers.toArray(new LispBuffer[buffers.size()]);
  }

  public List<LispWindow> getFrameWindows (LispFrame frame) {
    List<LispWindow> windows = new ArrayList<>();
    for (LispWindow window: myData) {
      if (window.getFrame() == frame)
        windows.add(window);
    }
    return windows;
  }

  /**
   * @return the last selected minibuffer window for given frame
   */
  @Nullable
  public LispWindow getFrameMinibufferWindow (LispFrame frame) {
    for (LispWindow window: myData) {
      if (window.getBuffer() instanceof LispMinibuffer && window.getFrame() == frame)
        return window;
    }
    return null;
  }

  @NotNull
  public LispWindow getEditorWindow (EditorWrapper editor) {
    for (LispWindow window: myData) {
      if (window.editorEquals(editor))
        return window;
    }
    throw new UnregisteredEditorException();
  }

  @Nullable
  public LispBuffer getFrameOtherBuffer (LispBuffer buffer, LispFrame frame, boolean invisibleBuffersPreferred) {
    LispBuffer spare = buffer;
    for (LispWindow window: myData) {
      if (window.getFrame() == frame && window.getBuffer() != buffer) {
        if ((invisibleBuffersPreferred && !window.isVisible()) || !invisibleBuffersPreferred)
          return window.getBuffer();
        if (spare == buffer)
          spare = window.getBuffer();
      }
    }
    //todo: in Emacs if there is no other, return *scratch*
    return spare;
  }

  public void hideBufferOnFrame(LispBuffer buffer, LispFrame frame) {
    List<LispWindow> windows = getBufferWindowsOnFrame(buffer, frame);
    if (windows.isEmpty() || (windows.size() == 1 && !windows.get(0).isVisible())) //already closed
      return;
    windows.get(0).close();
    for (int i = 1, windowsSize = windows.size(); i < windowsSize; i++) {
      LispWindow window = windows.get(i);
      window.closeTab();
      myData.remove(window);
    }
  }

  public List<LispWindow> getVisibleWindows(LispFrame frame) {
    List<LispWindow> visible = new ArrayList<>();
    for (LispWindow w: getFrameWindows(frame)) {
      if (w.isVisible()) {
        visible.add(w);
      }
    }
    return visible;
  }
}



