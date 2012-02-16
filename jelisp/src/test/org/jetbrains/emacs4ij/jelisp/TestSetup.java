package org.jetbrains.emacs4ij.jelisp;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/16/12
 * Time: 12:26 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class TestSetup {

    public static void runBeforeClass() {
        GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
        GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.2");
        GlobalEnvironment.initialize(null, null);
        GlobalEnvironment.INSTANCE.startRecording();
    }

    public static Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }
}
