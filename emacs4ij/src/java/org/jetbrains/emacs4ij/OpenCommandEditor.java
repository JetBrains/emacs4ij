package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.ui.EditorTextField;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenCommandEditor extends AnAction {

    private void evaluate () {

    }

    public void actionPerformed(AnActionEvent e) {
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        JComponent editorHeaderComponent = editor.getHeaderComponent();
        EditorTextField input = ServiceManager.getService(PluginService.class).getInput();
        if (editorHeaderComponent == null) {
            editor.setHeaderComponent(input);
            //registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, myInput);
        } else {
            //todo check for != !editor.getHeaderComponent().equals(myInput) => close that, open mine
            // todo
        }
        input.grabFocus();
    }
}
