package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/14/11
 * Time: 10:45 AM
 * To change this template use File | Settings | File Templates.
 */
public class AutoComplete extends AnAction {
    private String largestCommonPrefix (String s1, String s2) {
        String lcp = "";
        for (int i = 0; i != Math.min(s1.length(), s2.length()); ++i) {
            if (s1.charAt(i) == s2.charAt(i))
                lcp += s1.charAt(i);
            else
                break;
        }
        return lcp;
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if (!ApplicationManager.getApplication().getComponent(MyApplicationComponent.class).isGlobalEnvironmentInitialized())
            return;
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        try {
            IdeaMiniBuffer miniBuffer = (IdeaMiniBuffer) GlobalEnvironment.INSTANCE.getMiniBuffer();
            String parameter = miniBuffer.readInputString();
            List<String> completions = miniBuffer.getCompletions(parameter);
            if (completions.isEmpty()) {
                miniBuffer.setNoMatch(parameter);
            } else {
                if (completions.size() == 1) {
                    parameter = completions.get(0);
                } else {
                    parameter = largestCommonPrefix(completions.get(0), completions.get(completions.size()-1));
                }
                miniBuffer.setInputStartValue(parameter);
                miniBuffer.updateEditorText();

                if (completions.size()>1) {
                    String message = "Possible completions are:\n";
                    for (String name: completions) {
                        message += name + "\n";
                    }
                    Messages.showInfoMessage(message, "Possible completions");
                }
            }
        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), "Auto complete error");
        }
    }
}
