package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/3/11
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */
public class InterruptMiniBuffer extends AnAction {
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    @Override
    public void actionPerformed(AnActionEvent event) {
        GlobalEnvironment.INSTANCE.getMiniBuffer().kill();
    }
}
