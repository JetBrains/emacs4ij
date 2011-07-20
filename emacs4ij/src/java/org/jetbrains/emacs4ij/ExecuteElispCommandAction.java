package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;


/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class ExecuteElispCommandAction extends AnAction {
    public void actionPerformed(AnActionEvent e) {
        ExecuteElispCommandForm executeElispCommandForm = new ExecuteElispCommandForm();
        executeElispCommandForm.setVisible(true);
    }
}
