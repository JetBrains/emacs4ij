package org.jetbrains.emacs4ij;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.ui.Messages;
import com.intellij.util.ui.UIUtil;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/5/11
 * Time: 1:56 PM
 * To change this template use File | Settings | File Templates.
 */

public abstract class EmacsService  {
    protected String myEmacsParameter;

    private void showInfoMessage (final String name) {
        UIUtil.invokeLaterIfNeeded(new Runnable() {
            @Override
            public void run() {
                ApplicationManager.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        //Messages.showInfoMessage("You should choose Emacs " + name + " directory!", "Emacs4ij");
                        JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
                        fileChooser.setDialogTitle("Select Emacs " + name + "  directory");
                        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                        if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                            myEmacsParameter = fileChooser.getSelectedFile().getAbsolutePath();
                        } else {
                            Messages.showInfoMessage("You didn't choose Emacs " + name + " directory!\nUntil you set Emacs environment, no Emacs emulation will work.\nYou can set it by clicking on any of Emacs4ij icons.", "Emacs4ij");
                        }
                    }
                });
            }
        });
    }

    protected String reset (String name) {
        myEmacsParameter = "";
        showInfoMessage(name);
        return myEmacsParameter;
    }

    protected String checkSetEmacsParameter(String name) {
        if (myEmacsParameter == null || myEmacsParameter.equals("")) {
            myEmacsParameter = "";
            showInfoMessage(name);
        }
        return myEmacsParameter;
    }
}

