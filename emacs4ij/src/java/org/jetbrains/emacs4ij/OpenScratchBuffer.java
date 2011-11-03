package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenScratchBuffer extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        if (editor.isOneLineMode())
            return;
        String myName = Environment.ourScratchBufferName;
        String editorComponentName = editor.getContentComponent().getName();
        if (editorComponentName != null && editorComponentName.equals(myName))
            return;

        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        LispBuffer buffer = environment.findBuffer(myName);
        if (buffer == null)
            throw new NoBufferException(myName);

        IdeaEditor.headerOpened((IdeaEditor)buffer);
        buffer.setBufferActive();

    }
}
