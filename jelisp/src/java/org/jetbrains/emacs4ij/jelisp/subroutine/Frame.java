package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispInteger;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.elisp.Optional;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispWindow;

import java.util.ArrayList;
import java.util.List;

import static org.jetbrains.emacs4ij.jelisp.subroutine.Predicate.isNil;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 7:14 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class Frame {
  private Frame() {}

  @Subroutine("selected-frame")
  public static LispObject selectedFrame (Environment environment) {
    return Core.thisOrNil(environment.getSelectedFrame());
  }

  public static LispFrame getLiveFrame (Environment environment, LispObject object) {
    if (Predicate.isNil(object))
      object = environment.getSelectedFrame();
    LispSymbol frameLiveP = Predicate.frameLiveP(environment, object);
    if (frameLiveP.equals(LispSymbol.ourNil))
      throw new WrongTypeArgumentException("frame-live-p", object);
    return (LispFrame) object;
  }

  private static LispFrame getFrame (Environment environment, LispObject object) {
    if (Predicate.isNil(object))
      object = environment.getSelectedFrame();
    if (!(object instanceof LispFrame))
      throw new WrongTypeArgumentException("framep", object);
    return (LispFrame) object;
  }

  @Subroutine("frame-parameter")
  public static LispObject frameParameter (Environment environment, LispObject frame, LispSymbol parameter) {
//        System.out.println("Ask for frame parameter: " + parameter.getName());
    return (getFrame(environment, frame)).getParameter(parameter);
  }

  @Subroutine("get-buffer-window")
  public static LispObject getBufferWindow(Environment environment, @Optional LispObject bufferOrName, @Optional LispObject frame) {
    LispBuffer buffer = Buffer.getBufferByBufferNameOrNil(environment, bufferOrName);
    List<LispFrame> frames = new ArrayList<>();
    if (isNil(frame))
      frame = LispSymbol.ourNil;
    if (frame.equals(new LispSymbol("visible"))) {
      frames = environment.getVisibleFrames(); //search all visible frames
    } else if (frame.equals(LispSymbol.ourT)) { //search all frames.
      frames = environment.getAllFrames();
    } else if (frame.equals(new LispInteger(0))) { //search visible and iconified frames.
      frames = environment.getVisibleAndIconifiedFrames();
    } else if (frame instanceof LispFrame) { //search only that frame.
      frames.add((LispFrame) frame);
    } else {
      frames.add(environment.getSelectedFrame());
    }

    for (LispFrame f: frames) {
      LispWindow window = environment.getBufferWindowOnFrame(f, buffer);
      if (window != null && window.isVisible())
        return window;
    }

    return LispSymbol.ourNil;
  }

  @Subroutine(value = "make-frame-visible", isCmd = true)
  public static LispObject makeFrameVisible(Environment environment, @Optional LispObject frame) {
    getLiveFrame(environment, frame).setVisible(true);
    return frame;
  }

  @Subroutine(value = "make-frame-invisible", isCmd = true)
  public static LispObject makeFrameInvisible(Environment environment,
                                              @Optional LispObject frame, @Optional LispObject force) {
    LispFrame f = getLiveFrame(environment, frame);
    if (Predicate.isNil(force)) {
      //check that exists one more visible frame than that we want to hide
      int k = f.isVisible() ? 1 : 0;
      if (environment.getVisibleAndIconifiedFrames().size() - k <= 0) {
        Core.error(JelispBundle.message("make.invisible.error"));
        return LispSymbol.ourNil;
      }
    }
    f.setVisible(false);
    return LispSymbol.ourNil;
  }

  @Subroutine(value = "iconify-frame", isCmd = true)
  public static LispObject iconifyFrame(Environment environment, @Optional LispObject frame) {
    getLiveFrame(environment, frame).setIconified(true);
    return LispSymbol.ourNil;
  }

  @Subroutine("frame-list")
  public static LispList frameList (Environment environment) {
    List<LispFrame> frames = environment.getAllFrames();
    return LispList.list(frames.toArray(new LispObject[frames.size()]));
  }

  @Subroutine("frame-selected-window")
  public static LispWindow frameSelectedWindow (Environment environment, @Optional LispObject frame) {
    return environment.getFrameSelectedWindow(getLiveFrame(environment, frame));
  }

  @Subroutine("minibuffer-window")
  public static LispObject minibufferWindow (Environment environment, @Optional LispFrame frame) {
    return Core.thisOrNil(environment.getFrameMinibuffer(getLiveFrame(environment, frame)));
  }

  @Subroutine("modify-frame-parameters")
  public static LispSymbol modifyFrameParameters(Environment environment, LispObject frameObject, LispList parameters) {
    LispFrame frame = getLiveFrame(environment, frameObject);
    LispObject tail;
    for (tail = parameters; tail instanceof LispList; tail = ((LispList) tail).cdr()) {
      LispObject pair = ((LispList) tail).car();
      if (!(pair instanceof LispList))
        throw new WrongTypeArgumentException("listp", pair);
      LispObject name = ((LispList) pair).car();
      if (!(name instanceof LispSymbol))
        throw new WrongTypeArgumentException("symbolp", name);
      frame.setParameter((LispSymbol) name, ((LispList) pair).cdr());
    }
    return LispSymbol.ourNil;
  }

  @Subroutine("frame-parameters")
  public static LispList frameParameters(Environment environment, @Optional LispObject frameObject) {
    LispFrame frame = getFrame(environment, frameObject);
    return frame.getParameters();
  }
}
