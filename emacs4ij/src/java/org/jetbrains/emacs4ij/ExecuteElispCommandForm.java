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
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

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
    private JLabel commandLabel;

    public ExecuteElispCommandForm() {
        setTitle("Execute elisp command");
        setResizable(false);
        Dimension formSize = new Dimension(400, 100);
        setPreferredSize(formSize);
        setLocationRelativeTo(getOwner());
        commandLabel.setFocusable(false);
        commandTextArea.setWrapStyleWord(true);
        commandTextArea.setLineWrap(true);
        commandScrollPane.setViewportView(commandTextArea);
        this.add(mainPanel);
        pack();

        executeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                Parser parser = new Parser();
                LispObject lispObject = Evaluator.evaluate(parser.parseLine(commandTextArea.getText()), Environment.ourGlobal);
                Messages.showInfoMessage(lispObject.toString(), "Evaluation result");
            }
        });

    }


}
