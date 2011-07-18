package org.jetbrains.emacs4ij.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class NewElispProjectAction extends AnAction {
    public void actionPerformed(AnActionEvent e) {
        Application application = ApplicationManager.getApplication();
        NewElispProjectAppComponent newElispProjectAppComponent = application.getComponent(NewElispProjectAppComponent.class);
        newElispProjectAppComponent.createNewProject();
    }
}
