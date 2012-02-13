package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ApplicationComponent;
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

    public MyApplicationComponent() {
        myWindowAdapter = new WindowAdapter() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
                super.windowGainedFocus(e);
                if (EnvironmentInitializer.isGlobalInitialized())
                    GlobalEnvironment.INSTANCE.setSelectedFrame(new IdeaFrame((IdeFrameImpl) e.getWindow()));
            }

            @Override
            public void windowIconified(WindowEvent e) {
                super.windowIconified(e);
                //GlobalEnvironment.setFrameVisible(new IdeaFrame((IdeFrame) e.getWindow()), false);
                if (EnvironmentInitializer.isGlobalInitialized())
                    GlobalEnvironment.setFrameIconified(new IdeaFrame((IdeFrameImpl) e.getWindow()), true);
            }

            @Override
            public void windowDeiconified(WindowEvent e) {
                super.windowDeiconified(e);
                if (EnvironmentInitializer.isGlobalInitialized())
                    GlobalEnvironment.setFrameIconified(new IdeaFrame((IdeFrameImpl) e.getWindow()), false);
            }
        };
    }

    public void initComponent() {
        //   GlobalEnvironment.setSelectedFrame(new IdeaFrame((IdeFrameImpl) WindowManager.getInstance().getAllFrames()[0]));

        WindowManager.getInstance().addListener(new WindowManagerListener() {
            @Override
            public void frameCreated(IdeFrame ideFrame) {
                if (!EnvironmentInitializer.isGlobalInitialized())
                    return;
                ((IdeFrameImpl)ideFrame).addWindowFocusListener(myWindowAdapter);
                ((IdeFrameImpl)ideFrame).addWindowListener(myWindowAdapter);
                IdeaFrame ideaFrame = new IdeaFrame((IdeFrameImpl) ideFrame);
                GlobalEnvironment.INSTANCE.onFrameOpened(ideaFrame);
                //GlobalEnvironment.setSelectedFrame(ideaFrame);
            }

            @Override
            public void beforeFrameReleased(IdeFrame ideFrame) {
                if (!EnvironmentInitializer.isGlobalInitialized())
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