package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateCode extends AnAction {
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    public void actionPerformed(AnActionEvent e) {
        CustomEnvironment environment;
        try {
            environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        } catch (NullPointerException exc) {
            return;
        }
        if (environment == null)
            return;
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        try {
            LispBuffer buffer = GlobalEnvironment.INSTANCE.getBufferCurrentForEditing();
            LispObject result = buffer.evaluateLastForm();
            Messages.showInfoMessage(result.toString(), Emacs4ijBundle.message("evaluation.result.title"));
        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), Emacs4ijBundle.message("evaluation.result.title"));
        }
    }
}
