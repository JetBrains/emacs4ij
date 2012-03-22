package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.KeyboardShortcut;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

import javax.swing.*;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/15/12
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class EmacsAction extends AnAction {
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    public void actionPerformed(AnActionEvent e) {
        InputEvent inputEvent = e.getInputEvent();
        if (!(inputEvent instanceof KeyEvent)) {
            //i don't deal with mouse events yet
            return;
        }
        Environment environment;
        try {
            environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        } catch (NullPointerException exc) {
            return;
        }
        LispKeymap activeKeymap = GlobalEnvironment.INSTANCE.getActiveKeymap();
        KeyStroke keyStroke = KeyStroke.getKeyStrokeForEvent((KeyEvent) inputEvent);
        //todo: shortcuts with second keystroke?
        KeyboardShortcut shortcut = new KeyboardShortcut(keyStroke, null);
        LispSymbol action = activeKeymap.getKeyBinding(shortcut);
        try {
            if (action.equals(LispSymbol.ourNil))
                throw new InternalError("KeyDefinition is not a symbol!");
            BuiltinsCore.callInteractively(environment, action, null, null);
        } catch (Exception exc2) {
            Messages.showErrorDialog(exc2.getMessage(), Emacs4ijBundle.message("evaluation.error.title"));
        }
    }
}
