package org.jetbrains.emacs4ij;

import com.intellij.openapi.wm.IdeFrame;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaFrame implements LispFrame {
    private String myId;
    private IdeFrame myFrame;

    
    public IdeaFrame(IdeFrame frame) {
        //IdeFrame[] allFrames = WindowManager.getInstance().getAllFrames();
        //myFrame = WindowManager.getInstance().getIdeFrame(project);
        myFrame = frame;
    }


    @Override
    public String toString() {
        return "#<frame " + myFrame.toString() + ">";
    }

    @Override
    public LObject evaluate(Environment environment) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LObject invokeMethod(String methodName, Class[] parameterTypes, Object... methodParameters) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
