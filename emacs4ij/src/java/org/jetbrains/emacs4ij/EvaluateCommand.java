package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispMiniBuffer;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateCommand extends AnAction {

    public void actionPerformed(AnActionEvent e) {

        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());

        if (editor == null)
            return;

        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        if (!emacsHomeService.checkSetEmacsHome())
            return;

        String parameterValue = editor.getDocument().getText();
        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        String bufferName = environment.getBufferCurrentForEditing().getName();

        if (bufferName.equals(Environment.ourScratchBufferName)) {
            try {
                Parser parser = new Parser();
                LObject result = parser.parseLine(parameterValue).evaluate(environment);
                Messages.showInfoMessage(result.toString(), "Evaluation result");
            } catch (RuntimeException exc) {
                Messages.showErrorDialog(exc.getMessage(), "Evaluation result");
            }
            //todo close header
        } else if (bufferName.equals(Environment.ourMiniBufferName)) {
            try {
                LispMiniBuffer miniBuffer = (LispMiniBuffer) environment.findBuffer(Environment.ourMiniBufferName);
                if (miniBuffer == null) {
                    throw new RuntimeException("execute mini buffer command in nowhere!");
                }
                LObject result = miniBuffer.onReadInput();
                if (result != null)
                    Messages.showInfoMessage(result.toString(), "Evaluation result");
                //todo close header
            } catch (RuntimeException exc) {
                Messages.showErrorDialog(exc.getMessage(), "Evaluation result");
            }
        }
    }
}
