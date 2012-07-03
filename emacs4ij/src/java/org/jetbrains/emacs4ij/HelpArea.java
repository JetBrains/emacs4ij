package org.jetbrains.emacs4ij;

import com.intellij.execution.impl.ConsoleViewUtil;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.DataProvider;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.openapi.editor.ex.FocusChangeListener;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.SimpleToolWindowPanel;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.EditorWrapper;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispFrame;
import org.jetbrains.emacs4ij.jelisp.platform_dependent.LispToolWindow;
import org.jetbrains.emacs4ij.jelisp.subroutine.Buffer;

import javax.swing.*;
import java.awt.*;

/**
 * Created with IntelliJ IDEA.
 * User: kate
 * Date: 6/1/12
 * Time: 12:03 PM
 * To change this template use File | Settings | File Templates.
 */
public class HelpArea extends SimpleToolWindowPanel implements DataProvider, Disposable, LispToolWindow {
    private final Project myProject;
    private final Editor myEditor;
    private LispBuffer myBuffer = null;
    private int myId;
    private LispFrame myFrame = null;

    public HelpArea (final Project project) {
        super(false, true);
        myProject = project;
        myEditor = ConsoleViewUtil.setupConsoleEditor(myProject, false, false);
        ((EditorEx) myEditor).addFocusListener(new FocusChangeListener() {
            @Override
            public void focusGained(Editor editor) {
                GlobalEnvironment.INSTANCE.switchToWindow(new IdeaEditorWrapper(myEditor), true);
            }

            @Override
            public void focusLost(Editor editor) {
            }
        });
        setToolbar(new JPanel(new BorderLayout()));
        setContent(myEditor.getComponent());
        setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    public void setToolWindowEnabled (final boolean state, final Environment environment) {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                if (state && myBuffer == null) {
                    myBuffer = Buffer.getToolBufferCreate(environment, Emacs4ijBundle.message("help.buffer.name"), HelpArea.this);
                }
                ToolWindow toolWindow = ToolWindowManager.getInstance(myProject).getToolWindow("Emacs4ij Help");
                toolWindow.setAvailable(state, null);
            }
        });
    }

    private void releaseEditor() {
        if (myBuffer != null) {
            myBuffer.getEnvironment().killBuffer(myBuffer);
            myBuffer = null;
        }
        if (myEditor != null) {
            EditorFactory.getInstance().releaseEditor(myEditor);
//            myEditor = null;
        }
    }

    @Override
    public void dispose() {
        releaseEditor();
//        myProject = null;
    }

    @Override
    public EditorWrapper getEditor() {
        return new IdeaEditorWrapper(myEditor);
    }

    @Override
    public LispFrame getFrame() {
        return myFrame;
    }

    @Override
    public LispBuffer getBuffer() {
        return myBuffer;
    }

    @Override
    public void setActive() {
        if (myBuffer == null)
            throw new NoBufferException("*Help*");
        open();
    }

    @Override
    public void close() {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                ToolWindow toolWindow = ToolWindowManager.getInstance(myProject).getToolWindow("Emacs4ij Help");
                toolWindow.hide(null);
            }
        });
    }

    @Override
    public void open(@NotNull EditorWrapper wrapper) {
        throw new UnsupportedOperationException(Emacs4ijBundle.message("help.area.open.with.window"));
    }

    private void open() {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                ToolWindow toolWindow = ToolWindowManager.getInstance(myProject).getToolWindow("Emacs4ij Help");
                toolWindow.show(null);
            }
        });
    }

    @Override
    public void closeTab() {
        close();
    }

    @Override
    public boolean editorEquals(EditorWrapper editor) {
        return myEditor == ((IdeaEditorWrapper)editor).getEditor();
    }

    @Override
    public Integer getDisplayStart() {
        return 1;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    @Override
    public void setId(int id) {
        myId = id;
    }

    @Override
    public void setFrame(@NotNull LispFrame frame) {
        myFrame = frame;
    }

    @Override
    public boolean isRegistered() {
        return myFrame != null;
    }

    @Override
    public String toString() {
        return "#<window " + myId + " on " + Emacs4ijBundle.message("help.buffer.name") + '>';
    }
}
