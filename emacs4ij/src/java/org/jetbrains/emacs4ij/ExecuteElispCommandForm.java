package org.jetbrains.emacs4ij;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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

    public ExecuteElispCommandForm() {
        setTitle("Execute elisp command");
        setResizable(false);
        Dimension formSize = new Dimension(300, 100);
        setPreferredSize(formSize);
        pack();

        executeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                //TODO: execute command
                throw new NotImplementedException();
            }
        });
    }


}
