package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.ui.EditorTextField;

import javax.swing.*;
import java.awt.event.KeyEvent;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenCommandEditor extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        //test if we are not already in emacs editor line
        if (editor.isOneLineMode())
            return;
        JComponent editorHeaderComponent = editor.getHeaderComponent();
        if (editorHeaderComponent == null) {
            EditorTextField input = new EditorTextField();
            editor.setHeaderComponent(input);
            EvaluateCommand command = new EvaluateCommand();
            command.registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, input);
            input.grabFocus();
        }
    }
}
