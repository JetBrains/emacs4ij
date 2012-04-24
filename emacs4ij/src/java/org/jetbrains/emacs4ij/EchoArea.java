package org.jetbrains.emacs4ij;

import com.intellij.execution.impl.ConsoleViewUtil;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.DataProvider;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.ScrollType;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.HighlighterLayer;
import com.intellij.openapi.editor.markup.HighlighterTargetArea;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.SimpleToolWindowPanel;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 4/11/12
 * Time: 6:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class EchoArea extends SimpleToolWindowPanel implements DataProvider, Disposable {
    private Project myProject;
    private Editor myEditor;

    public EchoArea (final Project project) {
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
                ToolWindow toolWindow = ToolWindowManager.getInstance(myProject).getToolWindow("Emacs4ij Echo Area");
                toolWindow.setAvailable(state, null);
            }
        });
    }

    @Override
    public void dispose() {
        EditorFactory.getInstance().releaseEditor(myEditor);
        myProject = null;
        myEditor = null;
    }

    public void print (final String message, final TextAttributesKey key) {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                if (!myProject.isDisposed() && myEditor != null) {
                    Document document = myEditor.getDocument();
                    boolean scroll = document.getTextLength() == myEditor.getCaretModel().getOffset();
                    int messageStart = document.getTextLength();
                    document.insertString(document.getText().length(), message);

                    TextAttributes attributes = EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key);
                    int layer = HighlighterLayer.CARET_ROW + 1;
                    myEditor.getMarkupModel().addRangeHighlighter(messageStart, document.getTextLength(), layer, attributes,
                            HighlighterTargetArea.EXACT_RANGE);

                    if (scroll) {
                        myEditor.getCaretModel().moveToOffset(document.getTextLength());
                        myEditor.getScrollingModel().scrollToCaret(ScrollType.MAKE_VISIBLE);
                    }

                    ToolWindow toolWindow = ToolWindowManager.getInstance(myProject).getToolWindow("Emacs4ij Echo Area");
                    if (!toolWindow.isVisible())
                        toolWindow.show(null);
                }
            }
        });
    }
}
