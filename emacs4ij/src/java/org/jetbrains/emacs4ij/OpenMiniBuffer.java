package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/3/11
 * Time: 12:14 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenMiniBuffer extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        String myName = Environment.ourMiniBufferName;

        String editorComponentName = editor.getContentComponent().getName();
        if (editorComponentName != null && editorComponentName.equals(myName))
            return;

        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();

        IdeaMiniBuffer buffer = (IdeaMiniBuffer) environment.findBuffer(myName);
        if (buffer == null)
            throw new NoBufferException(myName);

        buffer.setReadCommandStatus();
        IdeaEditor.headerOpened(buffer);

        buffer.setBufferActive();

    }
}
