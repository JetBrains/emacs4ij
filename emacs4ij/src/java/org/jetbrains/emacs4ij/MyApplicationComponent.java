package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ApplicationComponent;
import com.intellij.openapi.wm.IdeFrame;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.openapi.wm.WindowManagerListener;
import com.intellij.openapi.wm.impl.IdeFrameImpl;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispFrame;

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

    public MyApplicationComponent() {
        myWindowAdapter = new WindowAdapter() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
                super.windowGainedFocus(e);
                if (!EnvironmentInitializer.isGlobalInitialized())
                    return;
                LispFrame frame = GlobalEnvironment.INSTANCE.getFrame((IdeFrameImpl) e.getWindow());
                GlobalEnvironment.INSTANCE.setSelectedFrame(frame);
            }

            @Override
            public void windowIconified(WindowEvent e) {
                super.windowIconified(e);
                if (!EnvironmentInitializer.isGlobalInitialized())
                    return;
                LispFrame frame = GlobalEnvironment.INSTANCE.getFrame((IdeFrameImpl) e.getWindow());
                frame.setIconified(true);
            }

            @Override
            public void windowDeiconified(WindowEvent e) {
                super.windowDeiconified(e);
                if (!EnvironmentInitializer.isGlobalInitialized())
                    return;
                LispFrame frame = GlobalEnvironment.INSTANCE.getFrame((IdeFrameImpl) e.getWindow());
                frame.setIconified(false);
            }
        };
    }

    public void initComponent() {
        WindowManager.getInstance().addListener(new WindowManagerListener() {
            @Override
            public void frameCreated(IdeFrame ideFrame) {
                if (!EnvironmentInitializer.isGlobalInitialized())
                    return;
                ((IdeFrameImpl)ideFrame).addWindowFocusListener(myWindowAdapter);
                ((IdeFrameImpl)ideFrame).addWindowListener(myWindowAdapter);
                IdeaFrame ideaFrame = new IdeaFrame((IdeFrameImpl) ideFrame);
                GlobalEnvironment.INSTANCE.onFrameOpened(ideaFrame);
            }

            @Override
            public void beforeFrameReleased(IdeFrame ideFrame) {
                if (!EnvironmentInitializer.isGlobalInitialized())
                    return;
                LispFrame frame = GlobalEnvironment.INSTANCE.getFrame(ideFrame);
                GlobalEnvironment.INSTANCE.onFrameReleased(frame);
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