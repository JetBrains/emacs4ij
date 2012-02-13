package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.IdeFrame;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.openapi.wm.impl.IdeFrameImpl;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 9:55 AM
 * To change this template use File | Settings | File Templates.
 */
public class EnvironmentInitializer {
    private static boolean isGlobalInitialized = false;

    public static boolean isGlobalInitialized() {
        return isGlobalInitialized;
    }

    public static boolean silentInitGlobal() {
        if (isGlobalInitialized)
            return true;
        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        EmacsSourceService emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        if (emacsHomeService.isParameterSet() && emacsSourceService.isParameterSet()) {
            isGlobalInitialized = (GlobalEnvironment.initialize(new BufferCreator(), new IdeProvider()) == 0);
        }
        return isGlobalInitialized;
    }

    public static boolean initGlobal() {
        if (isGlobalInitialized)
            return true;
        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        if (!emacsHomeService.checkSetEmacsHome())
            return false;
        EmacsSourceService emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        if (!emacsSourceService.checkSetEmacsSource())
            return false;
        int k;
        isGlobalInitialized = true;
        IdeProvider ideProvider = new IdeProvider();
        while ((k = GlobalEnvironment.initialize(new BufferCreator(), ideProvider)) != 0) {
            if (k == -1) {
                Messages.showInfoMessage("You might have mistaken when you set Emacs Home directory. Try again.", "Emacs4ij");
                if (!emacsHomeService.resetEmacsHome()) {
                    isGlobalInitialized = false;
                    break;
                }
                continue;
            }
            if (k == -2) {
                Messages.showInfoMessage("You might have mistaken when you set Emacs Source directory. Try again.", "Emacs4ij");
                if (!emacsSourceService.resetEmacsSource()) {
                    isGlobalInitialized = false;
                    break;
                }
                continue;
            }
            isGlobalInitialized = false;
            break;
        }
        return isGlobalInitialized;
    }

    public static void initProjectEnv (final Project project, final CustomEnvironment environment) {
        WindowManager windowManager = WindowManager.getInstance();
        for (IdeFrame frame: windowManager.getAllFrames()) {
            GlobalEnvironment.INSTANCE.onFrameOpened(new IdeaFrame((IdeFrameImpl) frame));
            if (((IdeFrameImpl) frame).hasFocus())
                GlobalEnvironment.INSTANCE.setSelectedFrame(new IdeaFrame((IdeFrameImpl) frame));
        }

        if (windowManager.getAllFrames().length > 0)
            GlobalEnvironment.INSTANCE.setSelectedFrame(new IdeaFrame((IdeFrameImpl) WindowManager.getInstance().getAllFrames()[0]));

        //environment = new CustomEnvironment(GlobalEnvironment.INSTANCE);
        IdeaMiniBuffer miniBuffer = new IdeaMiniBuffer(0, null, environment);
        environment.defineServiceBuffer(miniBuffer);
        String scratchDir = project.getProjectFilePath().substring(0, project.getProjectFilePath().lastIndexOf("/")+1);
        IdeaBuffer scratchBuffer = new IdeaBuffer(environment, GlobalEnvironment.ourScratchBufferName, scratchDir, null);
        environment.defineServiceBuffer(scratchBuffer);

        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runReadAction(new Runnable() {
                    @Override
                    public void run() {
                        final FileEditorManager fileEditorManager = FileEditorManager.getInstance(project);
                        for (final VirtualFile virtualFile : fileEditorManager.getOpenFiles()) {
                            ApplicationManager.getApplication().runReadAction(new Runnable() {
                                @Override
                                public void run() {
                                    IdeaBuffer newBuffer = new IdeaBuffer(environment, virtualFile.getName(), virtualFile.getParent().getPath() + '/', fileEditorManager.getSelectedTextEditor());
                                    environment.defineBuffer(newBuffer);
                                    //System.out.print("open: ");
                                    //setHeaders(newBuffer);
                                    //environment.printBuffers();
                                }
                            });
                        }

                    }
                });
            }
        });
    }
}
