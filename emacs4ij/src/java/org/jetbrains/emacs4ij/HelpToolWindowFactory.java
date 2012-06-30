package org.jetbrains.emacs4ij;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 6/1/12
 * Time: 5:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class HelpToolWindowFactory implements ToolWindowFactory {
    @Override
    public void createToolWindowContent(Project project, ToolWindow toolWindow) {
        HelpArea helpArea = project.getComponent(MyProjectComponent.class).getHelpArea();
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        final Content content = contentFactory.createContent(helpArea, "", false);
        toolWindow.getContentManager().addContent(content);
    }
}