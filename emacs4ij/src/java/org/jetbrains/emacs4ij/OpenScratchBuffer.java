package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.impl.EditorImpl;
import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispBuffer;
import org.jetbrains.emacs4ij.jelisp.exception.NoBufferException;

import java.awt.event.KeyEvent;

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
        if (!Checker.isReady())
            return;
        PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).initEnvironment();
        String myName = GlobalEnvironment.ourScratchBufferName;
        String editorComponentName = editor.getContentComponent().getName();
        if (editorComponentName != null && editorComponentName.equals(myName))
            return;

        Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        LispBuffer scratch = environment.getServiceBuffer(myName);
        if (scratch == null)
            throw new NoBufferException(myName);

        EditorTextField input = new EditorTextField();
        editor.setHeaderComponent(input);

        String name = ((EditorImpl)editor).getVirtualFile().getName();
        LispBuffer parent = environment.findBuffer(name);
        parent.setEditor(editor);
        environment.updateBuffer(parent);

        scratch.setEditor(input.getEditor());

        EvaluateCode command = new EvaluateCode();
        command.registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, input);

        scratch.setBufferActive();
    }
}
