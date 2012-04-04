package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinPredicates;
import org.jetbrains.emacs4ij.jelisp.subroutine.BuiltinsCore;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/15/12
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class EmacsAction extends AnAction {
    private KeymapCell myCommand = null;
    
    public EmacsAction () {}
    
    public EmacsAction (KeymapCell command) {
        myCommand = command;        
    }
    
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if (BuiltinPredicates.isNil(myCommand))
            throw new LispException(Emacs4ijBundle.message("emacs.action.nocommand.error", e.toString()));
        Environment environment;
        try {
            environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        } catch (NullPointerException exc) {
            return;
        }
        try {
            if (myCommand instanceof LispKeymap) {
                Messages.showInfoMessage("Long keystrokes are not supported yet", Emacs4ijBundle.message("evaluation.result.title"));
                return;
            }
            if (!((LispSymbol)myCommand).isFunction()) {
                System.err.println("upload: " + ((LispSymbol)myCommand).getName());
                LispSymbol realCommand = environment.find(((LispSymbol)myCommand).getName());
                if (realCommand != null)
                    myCommand = realCommand;
            }
            BuiltinsCore.callInteractively(environment, (LispSymbol)myCommand, null, null);
        } catch (Exception exc2) {
            Messages.showErrorDialog(exc2.getMessage(), Emacs4ijBundle.message("evaluation.error.title"));
        }
    }

    @Override
    public String toString() {
        return "EmacsAction (" + myCommand.toString() + ")";
    }
}
