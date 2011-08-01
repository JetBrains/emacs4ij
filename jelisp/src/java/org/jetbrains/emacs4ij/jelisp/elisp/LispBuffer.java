package org.jetbrains.emacs4ij.jelisp.elisp;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.awt.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 3:06 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispBuffer extends NamedLispObject {
    private TextArea myData = new TextArea();

    public LispBuffer(String name) {
        myName = new LispSymbol(name);
    }

    public LispBuffer(String myName, TextArea myData, boolean editable, int caretPosition) {
        this.myName = new LispSymbol(myName);
        this.myData = myData;
        this.myData.setEditable(editable);
        this.myData.setCaretPosition(caretPosition);
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName());
    }

    @Override
    public LispObject evaluate(Object... parameters) {
        throw new NotImplementedException();
    }
}
