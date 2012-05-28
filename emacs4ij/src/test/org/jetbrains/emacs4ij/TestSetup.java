package org.jetbrains.emacs4ij;

import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/24/12
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class TestSetup {
    public static String setGlobalEnv() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
        GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.3");
        GlobalEnvironment.TEST = true;
        return "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    }

    public static String getCause (Throwable e) {
        if (e.getCause() == null)
            return e.getMessage();
        return getCause(e.getCause());
    }
}
