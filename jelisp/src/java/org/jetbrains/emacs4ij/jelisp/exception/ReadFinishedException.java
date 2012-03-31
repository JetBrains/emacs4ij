package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 6:59 PM
 * To change this template use File | Settings | File Templates.
 */
public class ReadFinishedException extends LispException {
    public ReadFinishedException () {
        super(JelispBundle.message("reader.end"));
    }
}
