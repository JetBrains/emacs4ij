package org.jetbrains.emacs4ij.ide.tool;

import com.intellij.execution.impl.ConsoleViewUtil;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.DataProvider;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.SimpleToolWindowPanel;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.jetbrains.emacs4ij.ide.EnvironmentInitializer;
import org.jetbrains.emacs4ij.jelisp.LogUtil;

import javax.swing.*;
import java.awt.*;

abstract class Emacs4ijToolWindow extends SimpleToolWindowPanel implements DataProvider, Disposable {
  private final Project myProject;
  private final Editor myEditor;

  protected abstract String getToolWindowName();

  public Emacs4ijToolWindow(Project project) {
    super(false, true);
    myProject = project;
    myEditor = ConsoleViewUtil.setupConsoleEditor(myProject, false, false);
    setToolbar(new JPanel(new BorderLayout()));
    setContent(myEditor.getComponent());
    setEnabled(EnvironmentInitializer.isGlobalInitialized());
  }

  public void setToolWindowEnabled (final boolean state) {
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override
      public void run() {
        getIdeaToolWindow().setAvailable(state, null);
      }
    });
  }

  protected ToolWindow getIdeaToolWindow() {
    return ToolWindowManager.getInstance(myProject).getToolWindow(getToolWindowName());
  }

  protected boolean isProjectDisposed() {
    return myProject.isDisposed();
  }

  protected Editor getEditor() {
    return myEditor;
  }

  @Override
  public void dispose() {
    if (myEditor != null && !myEditor.isDisposed()) {
      EditorFactory.getInstance().releaseEditor(myEditor);
      LogUtil.info("dispose " + getToolWindowName());
    }
    if (myEditor != null && myEditor.isDisposed()) {
      LogUtil.info("double dispose " + getToolWindowName());
    }
  }
}
