package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ui.Messages;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.emacs4ij.jelisp.elisp.Ide;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 11/24/11
 * Time: 3:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class IdeProvider implements Ide {
    public IdeProvider () {}

    @Override
    public void showErrorMessage(final String message) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        Messages.showErrorDialog(message, "Emacs4ij Error");
                    }
                });
            }
        });
    }

    @Override
    public void showMessage(final String message) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        Messages.showInfoMessage(message, "Emacs4ij Message");
                    }
                });
            }
        });
    }
}
