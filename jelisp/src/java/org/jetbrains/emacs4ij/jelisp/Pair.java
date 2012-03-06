package org.jetbrains.emacs4ij.jelisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/6/12
 * Time: 2:48 PM
 * To change this template use File | Settings | File Templates.
 */
public class Pair {
    private int myLength;
    private int myModifier;
    public Pair (int a, int b) {
        myLength = a;
        myModifier = b;
    }

    public int getLength() {
        return myLength;
    }

    public int getModifier() {
        return myModifier;
    }
}
