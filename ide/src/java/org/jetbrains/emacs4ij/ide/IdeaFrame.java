package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;

public final class IdeaFrame extends LispFrame {
  private final IdeFrameImpl myFrame;

  //for test
  IdeaFrame() {
    super();
    myFrame = null;
  }

  public IdeaFrame(IdeFrameImpl frame) {
    super();
    myFrame = frame;
  }

  @Override
  public String toString() {
    return "#<frame " + (myFrame == null ? "*test*" : myFrame.toString()) + '>';
  }

  public void setVisible(boolean visible) {
    setParameter("visibility", visible ? LispSymbol.ourT : LispSymbol.ourNil);
    myFrame.setVisible(visible);
  }

  @Override
  public void setIconified(boolean iconified) {
    setParameter("visibility", iconified ? new LispSymbol("icon") : LispSymbol.ourT);
    if (!iconified) {
      myFrame.show();
      //WindowManagerImpl.getInstance().
    }  else {
      //  myFrame.setFocusableWindowState(false);

      // myFrame.setVisible(false);
      // TODO: really iconify =)

    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    IdeaFrame ideaFrame = (IdeaFrame) o;

    if (myFrame != null ? !myFrame.equals(ideaFrame.myFrame) : ideaFrame.myFrame != null) return false;

    return true;
  }

  @Override
  public int hashCode() {
    return myFrame != null ? myFrame.hashCode() : 0;
  }
}
