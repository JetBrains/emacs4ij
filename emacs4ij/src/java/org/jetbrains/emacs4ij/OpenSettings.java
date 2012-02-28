package org.jetbrains.emacs4ij;

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
    @Override
    public void actionPerformed(AnActionEvent e) {
        actionPerformed(PlatformDataKeys.PROJECT.getData(e.getDataContext()));
    }
    
    public void actionPerformed(Project p) {
        OptionsForm optionsForm = new OptionsForm(p);
        optionsForm.setLocationRelativeTo(null);//(WindowManager.getInstance().getIdeFrame(p).getComponent());
        optionsForm.setVisible(true);
        optionsForm.pack();
    }
}
