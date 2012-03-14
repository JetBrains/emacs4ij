package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
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
    public void actionPerformed(AnActionEvent event) {
        Editor editor = PlatformDataKeys.EDITOR.getData(event.getDataContext());
        if (editor == null)
            return;
        try {
            MyProjectComponent projectComponent = PlatformDataKeys.PROJECT.getData(event.getDataContext()).getComponent(MyProjectComponent.class);
            CustomEnvironment environment = projectComponent.getEnvironment();
            IdeaMiniBuffer miniBuffer = (IdeaMiniBuffer) environment.getMiniBuffer();
            miniBuffer.hide();
            //todo: reduce minibuffer recursion depth
//            Messages.showInfoMessage("Quit " + miniBuffer.getName(), "MiniBuffer");
        } catch (NullPointerException e) {
            //skip
        }
    }
}
