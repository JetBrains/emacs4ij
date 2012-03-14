package org.jetbrains.emacs4ij;

import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaFrame implements LispFrame {
    private String myId;
    protected IdeFrameImpl myFrame;
    private HashMap<String, LispObject> myParameters = new HashMap<>();
    private ArrayList<LispWindow> myWindows = new ArrayList<>();
    
    public IdeaFrame(IdeFrameImpl frame) {
        //IdeFrame[] allFrames = WindowManager.getInstance().getAllFrames();
        //myFrame = WindowManager.getInstance().getIdeFrame(project);
        myFrame = frame;
        myParameters.put("visibility", LispSymbol.ourT);
      //  height, width, name, title, menu-bar-lines, buffer-list, buffer-predicate foreground-color, background-color,
      // background-mode, display-type, alpha (transparency) ...
    }


    @Override
    public String toString() {
        return "#<frame " + myFrame.toString() + ">";
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public LispObject getParameter(String parameter) {
        LispObject value = myParameters.get(parameter);
        if (value == null)
            throw new VoidVariableException(parameter);
        return value;
    }

    @Override
    public void setVisible(boolean visible) {
        myParameters.put("visibility", visible ? LispSymbol.ourT : LispSymbol.ourNil);
        myFrame.setVisible(visible);
    }

    @Override
    public void setIconified(boolean iconified) {
        myParameters.put("visibility", iconified ? new LispSymbol("icon") : LispSymbol.ourT);
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
    public boolean isIconified() {
        return myParameters.get("visibility").equals(new LispSymbol("icon"));
    }

    @Override
    public boolean areIdeFramesEqual(LispFrame frame) {
        return myFrame.equals(((IdeaFrame)frame).myFrame);
    }

    @Override
    public boolean isVisible() {
        return myParameters.get("visibility").equals(LispSymbol.ourT);
    }

    @Override
    public void openWindow (LispBuffer buffer) {
        myWindows.add(new IdeaWindow(myWindows.size(), buffer));
    }

    @Override
    public void closeWindow(LispBuffer buffer) {
        int i = 0;
        for (; i != myWindows.size(); ++i) {
            if (myWindows.get(i).containsBuffer(buffer))
                break;
        }
        if (i < myWindows.size())
            myWindows.remove(i);
    }

    @Override
    public List<LispBuffer> getBufferList() {
        ArrayList<LispBuffer> buffers = new ArrayList<>();
        for (LispWindow window: myWindows) {
            if (window.getBuffer() != null)
                buffers.add(window.getBuffer());
        }
        return buffers;
    }

    @Override
    public LispWindow containsBuffer (LispBuffer buffer) {
        for (LispWindow window: myWindows) {
            if (window.containsBuffer(buffer))
                return window;
        }
        return null;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdeaFrame)) return false;

        IdeaFrame ideaFrame = (IdeaFrame) o;

        if (myFrame != null ? !myFrame.equals(ideaFrame.myFrame) : ideaFrame.myFrame != null) return false;
        if (myId != null ? !myId.equals(ideaFrame.myId) : ideaFrame.myId != null) return false;
        if (myParameters != null ? !myParameters.equals(ideaFrame.myParameters) : ideaFrame.myParameters != null)
            return false;
        if (myWindows != null ? !myWindows.equals(ideaFrame.myWindows) : ideaFrame.myWindows != null) return false;
        
        return true;
    }

    @Override
    public int hashCode() {
        int result = myId != null ? myId.hashCode() : 0;
        result = 31 * result + (myFrame != null ? myFrame.hashCode() : 0);
        result = 31 * result + (myParameters != null ? myParameters.hashCode() : 0);
        result = 31 * result + (myWindows != null ? myWindows.hashCode() : 0);
        return result;
    }
}
