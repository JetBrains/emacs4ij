package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMiniBuffer;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/3/11
 * Time: 12:14 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenMiniBuffer extends AnAction {
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        Environment environment;
        MyProjectComponent projectComponent = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class);
        try {
            environment = projectComponent.getEnvironment();
        } catch (NullPointerException exc) {
            return;
        }
        if (environment == null)
            return;
        LispBuffer buffer = environment.getBufferCurrentForEditing();
        buffer.setActive();
        LispMiniBuffer miniBuffer = environment.getMiniBuffer();
        miniBuffer.open(buffer);
    }
}
