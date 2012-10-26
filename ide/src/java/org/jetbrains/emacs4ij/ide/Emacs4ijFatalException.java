package org.jetbrains.emacs4ij.ide;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/14/12
 * Time: 12:34 PM
 * To change this template use File | Settings | File Templates.
 */
public class Emacs4ijFatalException extends RuntimeException {
    public Emacs4ijFatalException (String message) {
        super(message);
    }
}
