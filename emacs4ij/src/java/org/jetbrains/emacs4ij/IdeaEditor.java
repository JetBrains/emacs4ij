package org.jetbrains.emacs4ij;

import com.intellij.openapi.editor.Editor;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispString;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 11:10 AM
 * To change this template use File | Settings | File Templates.
 */
public class IdeaEditor extends LispObject implements LispBuffer {
    private String myName;
    private Editor myEditor;

    public IdeaEditor (String name,Editor editor) {
        myName = name;
        myEditor = editor;
    }

    @Override
    public LispString toLispString() {
        throw new NotImplementedException();  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispObject evaluate(Environment environment) {
        throw new NotImplementedException();  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public String getName() {
        return myName;
    }

    @Override
    public int getSize() {
        return myEditor.getDocument().getTextLength();
    }

    @Override
    public int point() {
        return myEditor.logicalPositionToOffset(myEditor.getCaretModel().getLogicalPosition()) + 1;
    }

    @Override
    public int pointMin() {
        return 1;
    }

    @Override
    public int pointMax() {
        return getSize()+1;
    }

    @Override
    public int bufferEnd(double parameter) {
        return (parameter > 0) ? pointMax() : pointMin();
    }

    @Override
    public String gotoChar (int position) {
        String message = "";
        if (position < pointMin()) {
            position = pointMin();
            message = "Beginning of buffer";
        }
        else if (position > pointMax()) {
            position = pointMax();
            message = "End of buffer";
        }
        myEditor.getCaretModel().moveToOffset(position-1);
        return message;
    }

    @Override
    public String forwardChar (int shift) {
        return gotoChar(point() + shift);
    }
}
