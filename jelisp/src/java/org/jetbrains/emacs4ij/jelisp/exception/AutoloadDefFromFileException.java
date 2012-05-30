package org.jetbrains.emacs4ij.jelisp.exception;

import org.jetbrains.emacs4ij.jelisp.JelispBundle;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 5/29/12
 * Time: 10:34 AM
 * To change this template use File | Settings | File Templates.
 */
public class AutoloadDefFromFileException extends LispException{
    public AutoloadDefFromFileException(String fileName, String name) {
        super(JelispBundle.message("autoload.def.from.file.failed", name, fileName));
    }
}
