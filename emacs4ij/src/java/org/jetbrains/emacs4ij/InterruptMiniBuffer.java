package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/3/11
 * Time: 5:38 PM
 * To change this template use File | Settings | File Templates.
 */
public class InterruptMiniBuffer extends AnAction {
    @Override
    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());

        if (editor == null)
            return;

        CustomEnvironment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        IdeaMiniBuffer miniBuffer = (IdeaMiniBuffer) environment.getMiniBuffer();
        miniBuffer.hide();
        environment.updateServiceBuffer(miniBuffer);

        Messages.showInfoMessage("Quit " + miniBuffer.getName(), "MiniBuffer");
    }
}
