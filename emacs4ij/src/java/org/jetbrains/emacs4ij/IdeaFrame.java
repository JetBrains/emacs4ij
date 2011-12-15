package org.jetbrains.emacs4ij;

import com.intellij.openapi.wm.IdeFrame;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;

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
    private ArrayList<LispSymbol> myLocalVariables = new ArrayList<LispSymbol>();

    
    public IdeaFrame(IdeFrame frame) {
        //IdeFrame[] allFrames = WindowManager.getInstance().getAllFrames();
        //myFrame = WindowManager.getInstance().getIdeFrame(project);
        myFrame = frame;
      //  height, width, name, title, menu-bar-lines, buffer-list, buffer-predicate foreground-color, background-color,
      // background-mode, display-type, alpha (transparency) ...
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

    @Override
    public LObject getParameter(String parameter) {
        for (LispSymbol variable: myLocalVariables) {
            if (variable.getName().equals(parameter)) {
                return variable;
            }
        }
        throw new VoidVariableException(parameter);
    }
}
