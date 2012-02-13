package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ServiceManager;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/13/12
 * Time: 11:02 AM
 * To change this template use File | Settings | File Templates.
 */
public class OptionsForm extends JFrame {
    private JPanel panel1;
    private JTextField homeTextField;
    private JTextField srcTextField;
    private JButton testHomeButton;
    private JButton testSrcButton;
    private JButton applyButton;
    private JButton quitButton;
    private JButton browseHomeButton;
    private JLabel srcLabel;
    private JLabel homeLabel;
    private JButton browseSrcButton;
    private JLabel infoLabel;

    private EmacsHomeService emacsHomeService;
    private EmacsSourceService emacsSourceService;
    private MyProjectComponent myProject;

    private String getServiceParameter(String name, JLabel label) {
        JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
        fileChooser.setDialogTitle("Select Emacs " + name + " directory");
        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            label.setForeground(Color.black);
            return fileChooser.getSelectedFile().getAbsolutePath();
        } else {
            label.setForeground(Color.red);
            return "";
            //Messages.showInfoMessage("You didn't choose Emacs " + name + " directory!\nUntil you set Emacs environment, no Emacs emulation will work.\nYou can set it by clicking on any of Emacs4ij icons.", "Emacs4ij");
        }
    }

    public OptionsForm(MyProjectComponent project) {
        emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        myProject = project;

        testHomeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (GlobalEnvironment.INSTANCE.testEmacsHome()) {
                    infoLabel.setText("Emacs home directory successfully set");
                    homeLabel.setForeground(Color.black);
                } else {
                    infoLabel.setText("Emacs home directory set failed");
                    homeLabel.setForeground(Color.red);
                }
            }
        });
        browseHomeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                String home = getServiceParameter("home", homeLabel);
                homeTextField.setText(home);
                emacsHomeService.setEmacsParameter(home);
                GlobalEnvironment.setEmacsHome(home);
            }
        });
        testSrcButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (GlobalEnvironment.INSTANCE.testEmacsSource()) {
                    infoLabel.setText("Emacs source directory successfully set");
                    srcLabel.setForeground(Color.black);
                } else {
                    infoLabel.setText("Emacs source directory set failed");
                    srcLabel.setForeground(Color.red);
                }
            }
        });
        browseSrcButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                String src = getServiceParameter("source", srcLabel);
                srcTextField.setText(src);
                emacsSourceService.setEmacsParameter(src);
                GlobalEnvironment.setEmacsSource(src);
            }
        });
        quitButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                OptionsForm.this.dispose();
            }
        });
        applyButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                //backgroundable?
                if (EnvironmentInitializer.initGlobal() && myProject != null) {
                    myProject.initEnv();
                }
                OptionsForm.this.dispose();
            }
        });
    }
}
