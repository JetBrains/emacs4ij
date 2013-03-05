package org.jetbrains.emacs4ij.ide;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.emacs4ij.ide.tool.HelpArea;

public class HelpToolWindowFactory implements ToolWindowFactory {
  @Override
  public void createToolWindowContent(Project project, ToolWindow toolWindow) {
    HelpArea helpArea = project.getComponent(MyProjectComponent.class).getHelpArea();
    Content content = ContentFactory.SERVICE.getInstance().createContent(helpArea, "", false);
    toolWindow.getContentManager().addContent(content);
  }
}