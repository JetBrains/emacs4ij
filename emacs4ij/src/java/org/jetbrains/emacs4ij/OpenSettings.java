package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 10:59 AM
 * To change this template use File | Settings | File Templates.
 */
public class OpenSettings extends AnAction {
    @Override
    public void actionPerformed(AnActionEvent e) {
        OptionsForm optionsForm = new OptionsForm(PlatformDataKeys.PROJECT.getData(e.getDataContext()));
        optionsForm.setVisible(true);
        optionsForm.pack();
    }
}
