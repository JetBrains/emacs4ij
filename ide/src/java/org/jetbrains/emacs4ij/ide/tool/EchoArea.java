package org.jetbrains.emacs4ij.ide.tool;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.ScrollType;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.HighlighterLayer;
import com.intellij.openapi.editor.markup.HighlighterTargetArea;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;

public class EchoArea extends Emacs4ijToolWindow {

  public EchoArea(Project project) {
    super(project);
  }

  public void print(final String message, final TextAttributesKey key) {
    ApplicationManager.getApplication().invokeLater(new Runnable() {
      @Override
      public void run() {
        if (isProjectDisposed() || getEditor() == null) {
          return;
        }
        Document document = getEditor().getDocument();
        boolean scroll = document.getTextLength() == getEditor().getCaretModel().getOffset();
        int messageStart = document.getTextLength();
        document.insertString(document.getText().length(), message);

        TextAttributes attributes = EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key);
        int layer = HighlighterLayer.CARET_ROW + 1;
        getEditor().getMarkupModel().addRangeHighlighter(messageStart, document.getTextLength(), layer, attributes, HighlighterTargetArea.EXACT_RANGE);

        if (scroll) {
          getEditor().getCaretModel().moveToOffset(document.getTextLength());
          getEditor().getScrollingModel().scrollToCaret(ScrollType.MAKE_VISIBLE);
        }

        ToolWindow toolWindow = getIdeaToolWindow();
        if (!toolWindow.isVisible())
          toolWindow.show(null);
      }
    });
  }

  @Override
  protected String getToolWindowName() {
    return "Emacs4ij Echo Area";
  }
}
