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
        GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.2");
        return "/home/kate/JetBrains/emacs4ij/emacs4ij/src/testSrc/";
//        return "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    }
}
