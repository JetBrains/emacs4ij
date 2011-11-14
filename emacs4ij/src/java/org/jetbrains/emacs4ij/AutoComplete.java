package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/14/11
 * Time: 10:45 AM
 * To change this template use File | Settings | File Templates.
 */
public class AutoComplete extends AnAction {
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
            IdeaMiniBuffer miniBuffer = (IdeaMiniBuffer) environment.getMiniBuffer();
            String parameter = miniBuffer.readParameter();
            if (miniBuffer.getStatus().equals(IdeaMiniBuffer.MiniBufferStatus.READ_COMMAND)) {

            }

            LObject result = miniBuffer.onReadInput();
            if (result != null)
                Messages.showInfoMessage(result.toString(), "Possible completions");
        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), "Auto complete error");
        }
    }
}
