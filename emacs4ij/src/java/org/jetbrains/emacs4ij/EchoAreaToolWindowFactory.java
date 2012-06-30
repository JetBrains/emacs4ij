package org.jetbrains.emacs4ij;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/11/12
 * Time: 6:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class EchoAreaToolWindowFactory implements ToolWindowFactory {
    @Override
    public void createToolWindowContent(Project project, ToolWindow toolWindow) {
        EchoArea echoArea = project.getComponent(MyProjectComponent.class).getEchoArea();
        ContentFactory contentFactory = ContentFactory.SERVICE.getInstance();
        final Content content = contentFactory.createContent(echoArea, "", false);
        toolWindow.getContentManager().addContent(content);
    }
}
