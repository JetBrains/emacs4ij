package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ServiceManager;
import org.jetbrains.emacs4ij.jelisp.DefinitionLoader;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.TestMode;

public abstract class TestSetup {
    public static String setGlobalEnv() {
        TestMode.TEST = true;
        TestMode.EXTRACT_DOC = false;
        TestMode.LOAD_FILES = false;

        if (GlobalEnvironment.INSTANCE == null) {
            System.out.println("INIT GLOBAL ENV");
            GlobalEnvironment.setEmacsSource("/home/kate/Downloads/emacs-23.4");
            GlobalEnvironment.setEmacsHome("/usr/share/emacs/23.4");
            DefinitionLoader.initialize(ServiceManager.getService(EmacsIndexService.class).getEmacsIndex());
            GlobalEnvironment.initialize(new KeymapCreator(), new BufferCreator(), new WindowCreator(), new IdeProvider());
            GlobalEnvironment.setFrameManager(new TestFrameManagerImpl());
        }
        GlobalEnvironment.INSTANCE.startRecording();
        return "/home/kate/emacs4ij/emacs4ij/src/testSrc/";
    }

    public static String getCause (Throwable e) {
        if (e.getCause() == null)
            return e.getMessage();
        return getCause(e.getCause());
    }
}
