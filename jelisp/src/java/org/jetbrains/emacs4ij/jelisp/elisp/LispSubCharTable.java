package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/28/12
 * Time: 3:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispSubCharTable extends LispObject {
    private int myDepth; // == [1, 2, 3]
    private int myMinChar;
    private ArrayList<LObject> myContent; //size == 1

    @Override
    public LObject evaluate(Environment environment) {
        return null;  //To change body of implemented methods use File | Settings | File Templates.
    }
}
