package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispList;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispFrame;

import javax.swing.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/12/11
 * Time: 1:19 PM
 * To change this template use File | Settings | File Templates.
 */
public final class IdeaFrame implements LispFrame {
    private final IdeFrameImpl myFrame;
    private Map<LispSymbol, LispObject> myParameters = new HashMap<>();

    public IdeaFrame(IdeFrameImpl frame) {
        myFrame = frame;
        initParameters();
    }

    //for test
    IdeaFrame () {
        myFrame = null;
        initParameters();
    }

    private void initParameters() {
        setParameter("visibility", LispSymbol.ourT);
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

    private LispObject getParameter(String parameter) {
        return getParameter(new LispSymbol(parameter));
    }

    @Override
    public LispObject getParameter(LispSymbol parameter) {
        LispObject value = myParameters.get(parameter);
        if (value == null)
            throw new VoidVariableException(parameter.getName());
        return value;
    }

    @Override
    public void setParameter(LispSymbol name, LispObject value) {
        myParameters.put(name, value);
    }

    private void setParameter(String name, LispObject value) {
        myParameters.put(new LispSymbol(name), value);
    }

    @Override
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
    public boolean isIconified() {
        return getParameter("visibility").equals(new LispSymbol("icon"));
    }

    @Override
    public boolean isVisible() {
        return getParameter("visibility").equals(LispSymbol.ourT);
    }

    @Override
    public JComponent getComponent() {
        return myFrame.getComponent();
    }

    @Override
    public LispList getParameters() {
        List<LispObject> list = new ArrayList<>();
        for (Map.Entry<LispSymbol, LispObject> parameter: myParameters.entrySet()) {
            LispList item = LispList.list(parameter.getKey());
            item.append(parameter.getValue());
            list.add(item);
        }
        return LispList.list(list);
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
