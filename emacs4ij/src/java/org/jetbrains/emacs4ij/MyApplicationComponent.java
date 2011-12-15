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

    public MyApplicationComponent() {
        myWindowAdapter = new WindowAdapter() {
            @Override
            public void windowGainedFocus(WindowEvent e) {
                super.windowGainedFocus(e);
                System.out.println(e.getWindow().getName() + " GainedFocus");
                GlobalEnvironment.setSelectedFrame(new IdeaFrame((IdeFrame) e.getWindow()));
                //Messages.showInfoMessage("windowGainedFocus", "Yo");
            }

            @Override
            public void windowLostFocus(WindowEvent e) {
                super.windowLostFocus(e);
                System.out.println(e.getWindow().getName() + " LostFocus");
                //Messages.showInfoMessage("windowLostFocus", "Yo");
            }
        };

        if (!Checker.isReady()) {
            Checker.isGlobalEnvironmentInitialized = false;
        } else {
            initGlobalEnvironment();
        }
    }

    public static boolean initGlobalEnvironment() {
        if (Checker.isGlobalEnvironmentInitialized)
            return true;
        int k;
        while ((k = GlobalEnvironment.initialize(new BufferCreator(), new IdeProvider())) < 0) {
            //Checker.isEnvironmentInitialized = false;
            if (k == -1) {
                Messages.showInfoMessage("You might have mistaken when you set Emacs Home directory. Try again.", "Emacs4ij");
                EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
                if (!emacsHomeService.resetEmacsHome())
                    return false;
                continue;
            }
            if (k == -2) {
                Messages.showInfoMessage("You might have mistaken when you set Emacs Source directory. Try again.", "Emacs4ij");
                EmacsSourceService emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
                if (!emacsSourceService.resetEmacsSource())
                    return false;
                continue;
            }
            return false;
        }
        Checker.isGlobalEnvironmentInitialized = true;
        WindowManager windowManager = WindowManager.getInstance();
        for (IdeFrame frame: windowManager.getAllFrames()) {
            GlobalEnvironment.onFrameOpened(new IdeaFrame(frame));
        }
        //GlobalEnvironment.setSelectedFrame(new IdeaFrame(windowManager.getIdeFrame(myProject)));
        return true;
    }

    public void initComponent() {
        WindowManager.getInstance().addListener(new WindowManagerListener() {
            @Override
            public void frameCreated(IdeFrame ideFrame) {
                ((IdeFrameImpl)ideFrame).addWindowFocusListener(myWindowAdapter);
                GlobalEnvironment.onFrameOpened(new IdeaFrame(ideFrame));
               //Messages.showInfoMessage("Frame created", "Yo");
            }

            @Override
            public void beforeFrameReleased(IdeFrame ideFrame) {
                GlobalEnvironment.onFrameReleased(new IdeaFrame(ideFrame));
                //Messages.showInfoMessage("Frame released", "Yo");
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
