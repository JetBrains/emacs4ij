package org.jetbrains.emacs4ij;

import com.intellij.openapi.ui.Messages;
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
    public void showErrorMessage(String message) {
        Messages.showDialog(message, "Elisp error", new String[] {"ok"}, 0, Messages.getErrorIcon());
      //  Messages.showErrorDialog(message, "Elisp error");
    }

    @Override
    public void showMessage(String message) {
        Messages.showInfoMessage(message, "Elisp message");
    }
}
