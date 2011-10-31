package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenCommandEditor extends AnAction {
    public static final String ourScratch = "*scratch*";
    public static final String ourMiniBuffer = " *Minibuf-0*";

    private void openBuffer(Environment environment, String bufferName) {
        LispBuffer buffer = environment.findBuffer(bufferName);
        if (buffer == null)
            throw new NoBufferException(bufferName);

        if (buffer instanceof IdeaMiniBuffer)
            ((IdeaMiniBuffer)buffer).setReadCommandStatus();
        IdeaEditor.headerOpened((IdeaEditor)buffer);
        buffer.setBufferActive();
    }

    private void grabFocus (Environment environment, String bufferName) {
        LispBuffer buffer = environment.findBuffer(bufferName);
        if (buffer == null)
            throw new NoBufferException(bufferName);
        IdeaEditor.headerOpened((IdeaEditor)buffer);
        buffer.grabFocus();
    }

    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        JComponent editorHeaderComponent = editor.getHeaderComponent();
        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        String currentBufferName = environment.getBufferCurrentForEditing().getName();
        if (editorHeaderComponent == null) {
            if (currentBufferName.equals(ourScratch)) {
                openBuffer(environment, ourMiniBuffer);
            } else if (!(currentBufferName.equals(ourMiniBuffer))) {
                openBuffer(environment, ourScratch);
            }
        } else {
            if (currentBufferName.equals(ourScratch)) {
                grabFocus(environment, ourMiniBuffer);

            } else if (!(currentBufferName.equals(ourMiniBuffer))) {
                grabFocus(environment, ourScratch);
            }
        }

    }
}
