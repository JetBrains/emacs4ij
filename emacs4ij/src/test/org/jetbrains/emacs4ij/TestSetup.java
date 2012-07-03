package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ServiceManager;
import org.jetbrains.emacs4ij.jelisp.DefinitionLoader;
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
        if (GlobalEnvironment.INSTANCE == null) {
            System.out.println("INIT GLOBAL ENV");
            GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
            GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.3");
            DefinitionLoader.initialize(ServiceManager.getService(EmacsIndexService.class).getEmacsIndex());
            GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new WindowCreator(), new IdeProvider());
            GlobalEnvironment.setFrameManager(new TestFrameManagerImpl());
        }
        GlobalEnvironment.TEST = true;
        GlobalEnvironment.INSTANCE.startRecording();
        return "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    }


    public static String getCause (Throwable e) {
        if (e.getCause() == null)
            return e.getMessage();
        return getCause(e.getCause());
    }
}
