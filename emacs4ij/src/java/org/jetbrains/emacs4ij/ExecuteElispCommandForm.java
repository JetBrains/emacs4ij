package org.jetbrains.emacs4ij;

import com.intellij.openapi.ui.Messages;
import org.jetbrains.annotations.TestOnly;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Evaluator;
import org.jetbrains.emacs4ij.jelisp.Parser;
import org.jetbrains.emacs4ij.jelisp.elisp.LispObject;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:32 PM
 * To change this template use File | Settings | File Templates.
 */
public class ExecuteElispCommandForm extends JFrame implements Runnable {
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
    private JTextArea minibufferTextArea;
    private JScrollPane minibufferScrollPane;

    @TestOnly
    public void setCommandInteractive (String command) {
        emacsHomeTextArea.setText("c:\\Users\\ekaterina.polishchuk\\Downloads\\emacs-23.3");
        commandTextArea.setText("(interactive \"" + command + "\")");
    }

    public ExecuteElispCommandForm() {
        setTitle("Emacs4ij");
        setResizable(false);
        Dimension formSize = new Dimension(400, 225);
        setPreferredSize(formSize);
        setLocationRelativeTo(getOwner());
        commandLabel.setFocusable(false);
        commandTextArea.setWrapStyleWord(true);
        commandTextArea.setLineWrap(true);
        commandScrollPane.setViewportView(commandTextArea);
        emacsHomeTextArea.setText(Environment.ourEmacsPath);
        emacsHomeScrollPane.setViewportView(emacsHomeTextArea);
        minibufferScrollPane.setViewportView(minibufferTextArea);
        //Environment.ourMiniBuffer = minibufferTextArea;
        executeTab.setName("Execute");
        settingsTab.setName("Settings");
        commandTabbedPane.add(executeTab);
        commandTabbedPane.add(settingsTab);
        this.add(mainPanel);
        pack();

        minibufferTextArea.addKeyListener(new KeyListener() {
            public void keyTyped(KeyEvent e) {
            }

            public void keyPressed(KeyEvent e) {
            }

            public void keyReleased(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    //typed[0] = true;
                    System.out.println("hi");
                }

            }
        });

        executeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Parser parser = new Parser();
                try {
                    try {

                        Thread.currentThread().wait();
                    } catch (InterruptedException e1) {
                        LispObject lispObject = Evaluator.evaluate(parser.parseLine(commandTextArea.getText()), Environment.ourGlobal);
                        Messages.showInfoMessage(lispObject.toString(), "Evaluation result");
                    }

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
                //Environment.ourMiniBuffer = null;
                setVisible(false);
                dispose();
            }
        });

    }

    public void run() {
        this.setVisible(true);
    }
}
