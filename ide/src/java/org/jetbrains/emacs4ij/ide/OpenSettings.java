package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.project.Project;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 10:59 AM
 * To change this template use File | Settings | File Templates.
 */
public class OpenSettings extends AnAction {
    private OptionsForm myOptionsForm = null;

    @Override
    public void actionPerformed(AnActionEvent e) {
        actionPerformed(PlatformDataKeys.PROJECT.getData(e.getDataContext()));
    }

    public void actionPerformed(Project p) {
        if (myOptionsForm == null) {
            myOptionsForm = new OptionsForm(p);
            myOptionsForm.setLocationRelativeTo(null);
            myOptionsForm.setVisible(true);
            myOptionsForm.pack();
        } else {
            myOptionsForm.refreshText();
            myOptionsForm.setVisible(true);
            myOptionsForm.requestFocus();
        }
    }
}
