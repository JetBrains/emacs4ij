package org.jetbrains.emacs4ij;

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


    @Override
    public String toString() {
        return "#<frame >";
    }
}
