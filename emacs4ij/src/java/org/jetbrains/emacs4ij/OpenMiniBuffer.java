package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.ui.EditorTextField;
import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;

import java.awt.event.KeyEvent;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/3/11
 * Time: 12:14 PM
 * To change this template use File | Settings | File Templates.
 */
public class OpenMiniBuffer extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        CustomEnvironment environment;
        try {
            environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        } catch (NullPointerException exc) {
            return;
        }
        if (environment == null)
            return;
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());
        if (editor == null)
            return;
        if (editor.isOneLineMode()) {
            //todo: open new mini buffer over old ones and increase recursion depth
            return;
        }
        IdeaMiniBuffer miniBuffer = (IdeaMiniBuffer) environment.getMiniBuffer();
        EditorTextField input = new EditorTextField();
        editor.setHeaderComponent(input);

//        try {
//            String name = ((EditorImpl)editor).getVirtualFile().getName();
//            LispBuffer parent = environment.findBufferSafe(name);
//            parent.setEditor(editor);
//        } catch (NullPointerException exc) {
//            throw new RuntimeException("Open Minibuffer over buffer with no file bound!");
//        }

        miniBuffer.setEditor(input.getEditor());

        ExecuteCommand command = new ExecuteCommand();
        command.registerCustomShortcutSet(KeyEvent.VK_ENTER, 0, input);
        InterruptMiniBuffer imb = new InterruptMiniBuffer();
        imb.registerCustomShortcutSet(KeyEvent.VK_ESCAPE, 0, input);
        AutoComplete autoComplete = new AutoComplete();
        autoComplete.registerCustomShortcutSet(KeyEvent.VK_TAB, 0, input);

        miniBuffer.setBufferActive();
    }
}
