package org.jetbrains.emacs4ij;

import com.intellij.openapi.ui.Messages;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Evaluator;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class ExecuteElispCommandForm extends JFrame {
    private JTextArea commandTextArea;
    private JButton executeButton;
    private JScrollPane commandScrollPane;
    private JPanel mainPanel;
    private JPanel executeTab;
    private JPanel settingsTab;
    private JLabel commandLabel;
    private JTabbedPane commandTabbedPane;
    private JButton browseButton;
    private JTextArea emacsHomeTextArea;
    private JScrollPane emacsHomeScrollPane;

    public ExecuteElispCommandForm() {
        setTitle("Emacs4ij");
        setResizable(false);
        Dimension formSize = new Dimension(400, 200);
        setPreferredSize(formSize);
        setLocationRelativeTo(getOwner());
        commandLabel.setFocusable(false);
        commandTextArea.setWrapStyleWord(true);
        commandTextArea.setLineWrap(true);
        commandScrollPane.setViewportView(commandTextArea);
        emacsHomeTextArea.setText(Environment.ourEmacsPath);
        emacsHomeScrollPane.setViewportView(emacsHomeTextArea);
        executeTab.setName("Execute");
        settingsTab.setName("Settings");
        commandTabbedPane.add(executeTab);
        commandTabbedPane.add(settingsTab);
        this.add(mainPanel);
        pack();

        executeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Parser parser = new Parser();
                try {
                    LispObject lispObject = Evaluator.evaluate(parser.parseLine(commandTextArea.getText()), Environment.ourGlobal);
                    Messages.showInfoMessage(lispObject.toString(), "Evaluation result");
                } catch (RuntimeException exc) {
                    Messages.showErrorDialog(exc.getMessage(), "Elisp interpreter error");
                }
            }
        });

        browseButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
                fileChooser.setDialogTitle("Select Emacs home directory");
                fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                if (fileChooser.showOpenDialog(settingsTab) == JFileChooser.APPROVE_OPTION) {
                    String emacsHome = fileChooser.getSelectedFile().getAbsolutePath();
                    Environment.ourEmacsPath = emacsHome;
                    emacsHomeTextArea.setText(emacsHome);
                }
            }
        });

        this.addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(WindowEvent winEvt) {
                setVisible(false);
                dispose();
            }
        });

    }


}
