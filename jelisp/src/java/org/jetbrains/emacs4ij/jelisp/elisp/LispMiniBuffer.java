package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 3:01 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispMiniBuffer extends LispBuffer {
    public void readCommand (LObject defaultValue);
    public void readArgument (SpecialFormInteractive interactive);
    public LObject onReadInput ();
}
