package org.jetbrains.emacs4ij;

import com.intellij.openapi.wm.IdeFrame;
import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import javax.swing.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaFrame implements LispFrame {
    private final IdeFrameImpl myFrame;
    private Map<String, LispObject> myParameters = new HashMap<>();

    public IdeaFrame(IdeFrameImpl frame) {
        myFrame = frame;
        myParameters.put("visibility", LispSymbol.ourT);
        initParameters();
    }

    //for test
    IdeaFrame () {
        myFrame = null;
        initParameters();
    }

    @Override
    public IdeFrame getIdeFrame() {
        return myFrame;
    }

    private void initParameters() {
        setParameter("buffer-predicate", LispSymbol.ourNil);
    }

    @Override
    public String toString() {
        return "#<frame " + (myFrame == null ? "*test*" : myFrame.toString()) + '>';
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
    public void setParameter(String name, LispObject value) {
        myParameters.put(name, value);
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
    public boolean isVisible() {
        return myParameters.get("visibility").equals(LispSymbol.ourT);
    }

    @Override
    public JComponent getComponent() {
        return myFrame.getComponent();
    }
}
