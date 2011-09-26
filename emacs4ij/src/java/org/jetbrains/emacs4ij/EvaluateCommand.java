package org.jetbrains.emacs4ij;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LObject;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 6:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class EvaluateCommand extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        EmacsHomeService emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        Editor editor = PlatformDataKeys.EDITOR.getData(e.getDataContext());

        if (editor == null)
            return;

        if (emacsHomeService.getEmacsHome() == null || emacsHomeService.getEmacsHome().equals("")) {
            Messages.showInfoMessage("You should choose Emacs home directory!", "Emacs4ij");
            JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
            fileChooser.setDialogTitle("Select Emacs home directory");
            fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            if (fileChooser.showOpenDialog(editor.getComponent()) == JFileChooser.APPROVE_OPTION) {
                String emacsHome = fileChooser.getSelectedFile().getAbsolutePath();
                emacsHomeService.setEmacsHome(emacsHome);
                Environment.ourEmacsPath = emacsHome;
            } else {
                Messages.showErrorDialog("You didn't choose Emacs home directory!\nNo command evaluation will be done.", "Emacs4ij");
                //todo close header
                return;
            }
        } else {
            if (Environment.ourEmacsPath.equals(""))
                Environment.ourEmacsPath = emacsHomeService.getEmacsHome();
        }
        String parameterValue = editor.getDocument().getText();
        try {
            Environment environment = PlatformDataKeys.PROJECT.getData(e.getDataContext()).getComponent(MyProjectComponent.class).getEnvironment();
            Parser parser = new Parser();
            LObject lispObject = parser.parseLine(parameterValue).evaluate(environment);
            Messages.showInfoMessage(lispObject.toString(), "Evaluation result");
        } catch (RuntimeException exc) {
            Messages.showErrorDialog(exc.getMessage(), "Evaluation result");
        }
        //todo close header
    }
}
