package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ApplicationComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.wm.IdeFrame;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.openapi.wm.WindowManagerListener;
import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/15/11
 * Time: 2:00 PM
 * To change this template use File | Settings | File Templates.
 */
public class MyApplicationComponent implements ApplicationComponent {
    private WindowAdapter myWindowAdapter;
    private boolean isGlobalEnvironmentInitialized = false;

    public boolean isGlobalEnvironmentInitialized() {
        return isGlobalEnvironmentInitialized;
    }

    public MyApplicationComponent() {
        myWindowAdapter = new WindowAdapter() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
                super.windowGainedFocus(e);
                if (isGlobalEnvironmentInitialized)
                    GlobalEnvironment.INSTANCE.setSelectedFrame(new IdeaFrame((IdeFrameImpl) e.getWindow()));
            }

            @Override
            public void windowIconified(WindowEvent e) {
                super.windowIconified(e);
                //GlobalEnvironment.setFrameVisible(new IdeaFrame((IdeFrame) e.getWindow()), false);
                if (isGlobalEnvironmentInitialized)
                    GlobalEnvironment.setFrameIconified(new IdeaFrame((IdeFrameImpl) e.getWindow()), true);
            }

            @Override
            public void windowDeiconified(WindowEvent e) {
                super.windowDeiconified(e);
                if (isGlobalEnvironmentInitialized)
                    GlobalEnvironment.setFrameIconified(new IdeaFrame((IdeFrameImpl) e.getWindow()), false);
            }
        };        
    }

    public boolean silentGlobalEnvInit() {
        if (isGlobalEnvironmentInitialized)
            return true;
        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        EmacsSourceService emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        if (emacsHomeService.isParameterSet() && emacsSourceService.isParameterSet()) {
            isGlobalEnvironmentInitialized = (GlobalEnvironment.initialize(new BufferCreator(), new IdeProvider()) == 0);
        }
        return isGlobalEnvironmentInitialized;
    }
    
    public boolean globalEnvInit() {
        if (isGlobalEnvironmentInitialized)
            return true;
        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        if (!emacsHomeService.checkSetEmacsHome())
            return false;
        EmacsSourceService emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        if (!emacsSourceService.checkSetEmacsSource())
            return false;
        int k;
        isGlobalEnvironmentInitialized = true;
        IdeProvider ideProvider = new IdeProvider();
        while ((k = GlobalEnvironment.initialize(new BufferCreator(), ideProvider)) != 0) {
            if (k == -1) {
                Messages.showInfoMessage("You might have mistaken when you set Emacs Home directory. Try again.", "Emacs4ij");
                if (!emacsHomeService.resetEmacsHome()) {
                    isGlobalEnvironmentInitialized = false;
                    break;
                }
                continue;
            }
            if (k == -2) {
                Messages.showInfoMessage("You might have mistaken when you set Emacs Source directory. Try again.", "Emacs4ij");
                if (!emacsSourceService.resetEmacsSource()) {
                    isGlobalEnvironmentInitialized = false;
                    break;
                }
                continue;
            }
            isGlobalEnvironmentInitialized = false;
            break;
        }
        return isGlobalEnvironmentInitialized;
    }

    public void initComponent() {
        //   GlobalEnvironment.setSelectedFrame(new IdeaFrame((IdeFrameImpl) WindowManager.getInstance().getAllFrames()[0]));

        WindowManager.getInstance().addListener(new WindowManagerListener() {
            @Override
            public void frameCreated(IdeFrame ideFrame) {
                if (!isGlobalEnvironmentInitialized)
                    return;
                ((IdeFrameImpl)ideFrame).addWindowFocusListener(myWindowAdapter);
                ((IdeFrameImpl)ideFrame).addWindowListener(myWindowAdapter);
                IdeaFrame ideaFrame = new IdeaFrame((IdeFrameImpl) ideFrame);
                GlobalEnvironment.INSTANCE.onFrameOpened(ideaFrame);
                //GlobalEnvironment.setSelectedFrame(ideaFrame);
            }

            @Override
            public void beforeFrameReleased(IdeFrame ideFrame) {
                if (!isGlobalEnvironmentInitialized)
                    return;
                GlobalEnvironment.INSTANCE.onFrameReleased(new IdeaFrame((IdeFrameImpl) ideFrame));
            }
        });
    }

    public void disposeComponent() {
        // TODO: insert component disposal logic here
    }

    @NotNull
    public String getComponentName() {
        return "org.jetbrains.emacs4ij.MyApplicationComponent";
    }

}