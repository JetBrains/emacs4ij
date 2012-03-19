package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMiniBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/3/11
 * Time: 3:22 PM
 * To change this template use File | Settings | File Templates.
 */
public class ExecuteCommand extends AnAction {
    @Override
    public void actionPerformed(AnActionEvent e) {
        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        try {
            LispMiniBuffer miniBuffer = environment.getMiniBuffer();
            LispObject result = miniBuffer.onReadInput();
            if (result != null && miniBuffer.wasInteractiveFormResult())
                Messages.showInfoMessage(result.toString(), Emacs4ijBundle.message("evaluation.result.title"));
        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), Emacs4ijBundle.message("evaluation.result.title"));
        }
    }
}
