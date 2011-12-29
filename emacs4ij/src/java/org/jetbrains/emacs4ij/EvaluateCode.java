package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateCode extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        if (!Checker.isReady())
            return;
       // PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).initEnvironment();

        String parameterValue = editor.getDocument().getText();
        CustomEnvironment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();

        try {
            Parser parser = new Parser();
            String displayedBufferName = IdeaBuffer.getDisplayedBufferName();

            LObject result = parser.parseLine(parameterValue).evaluate(environment);
            Messages.showInfoMessage(result.toString(), "Evaluation result");

            environment.findBuffer(displayedBufferName).closeHeader();

        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), "Evaluation result");
        }
    }
}
