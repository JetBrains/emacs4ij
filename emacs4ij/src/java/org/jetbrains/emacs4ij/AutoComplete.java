package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

import java.util.ArrayList;

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
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());

        if (editor == null)
            return;

        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        if (!emacsHomeService.checkSetEmacsHome())
            return;

       // Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();

        try {
            IdeaMiniBuffer miniBuffer = (IdeaMiniBuffer) GlobalEnvironment.getInstance().getMiniBuffer();
            String parameter = miniBuffer.readParameter();
            if (miniBuffer.getStatus().equals(IdeaMiniBuffer.MiniBufferStatus.READ_COMMAND)) {
                ArrayList<String> commandNames = GlobalEnvironment.getInstance().getCommandList(parameter);
                if (commandNames.isEmpty()) {
                    miniBuffer.onReadInput();
                } else {
                    if (commandNames.size() == 1) {
                        parameter = commandNames.get(0);
                    } else {
                        parameter = largestCommonPrefix(commandNames.get(0), commandNames.get(commandNames.size()-1));
                    }
                    miniBuffer.readCommand(null, parameter, false);
                    String message = "Possible completions are:\n";
                    for (String name: commandNames) {
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
