package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.elisp.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;
import org.jetbrains.emacs4ij.jelisp.elisp.LispSymbol;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 3/15/12
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class EmacsAction extends AnAction {
    private LispObject myCommand = null;
    
    public EmacsAction () {}
    
    public EmacsAction (LispObject command) {
        myCommand = command;
    }
    
    public void update(AnActionEvent event) {
        event.getPresentation().setEnabled(EnvironmentInitializer.isGlobalInitialized());
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        if (Predicate.isNil(myCommand))
            throw new LispException(Emacs4ijBundle.message("emacs.action.nocommand.error", e.toString()));
        Environment environment;
        try {
            environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
        } catch (NullPointerException exc) {
            return;
        }
        try {
            if (myCommand instanceof LispKeymap) {
                GlobalEnvironment.echo(JelispBundle.message("not.supported", "long keystrokes"), GlobalEnvironment.MessageType.WARNING);
                return;
            }
            if (myCommand instanceof LispSymbol) {
                if (!((LispSymbol)myCommand).isFunction()) {
                    LispSymbol cmd = environment.find(((LispSymbol)myCommand).getName());
                    if (cmd == null || !cmd.isFunction()) {
                        System.err.println("upload: " + ((LispSymbol)myCommand).getName());
                        LispSymbol realCommand = environment.findAndRegisterEmacsFunction(((LispSymbol)myCommand).getName());
                        if (realCommand != null)
                            myCommand = realCommand;
                    } else
                        myCommand = cmd;
                }
                Core.callInteractively(environment, myCommand, null, null);
            } else {
                GlobalEnvironment.echo(Emacs4ijBundle.message("not.cmd", myCommand.toString()), GlobalEnvironment.MessageType.ERROR);
            }

        } catch (Exception exc2) {
            exc2.printStackTrace();
            GlobalEnvironment.echo("Emacs4ij action:\n" + exc2.getMessage(), GlobalEnvironment.MessageType.ERROR);
        }
    }

    @Override
    public String toString() {
        return "EmacsAction (" + myCommand.toString() + ")";
    }
}
