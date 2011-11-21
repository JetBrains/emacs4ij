package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;

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
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());

        if (editor == null)
            return;

        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        if (!emacsHomeService.checkSetEmacsHome())
            return;

        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();

        try {
            environment.getMiniBuffer().onReadInput();
        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), "Evaluation result");
        }
    }
}
