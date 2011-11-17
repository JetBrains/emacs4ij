package org.jetbrains.emacs4ij.jelisp.elisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/23/11
 * Time: 3:01 PM
 * To change this template use File | Settings | File Templates.
 */
public interface LispMiniBuffer extends LispBuffer {
    public void startRead ();
    public void readParameter (SpecialFormInteractive interactive);
    public LObject onReadInput ();
    public LObject onInteractiveNoIoInput(SpecialFormInteractive interactive);
}
