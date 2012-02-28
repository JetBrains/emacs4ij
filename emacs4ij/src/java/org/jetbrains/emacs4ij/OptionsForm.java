package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.annotations.NotNull;
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
    private JButton applyButton;
    private JButton quitButton;
    private JButton browseHomeButton;
    private JLabel srcLabel;
    private JLabel homeLabel;
    private JButton browseSrcButton;
    private JLabel infoLabel;

    private EmacsHomeService emacsHomeService;
    private EmacsSourceService emacsSourceService;
    private MyProjectComponent myProjectComponent;
    private Project myProject;

    private String wasHome = null;
    private String wasSrc = null;
    private boolean isHomeValid = true;
    private boolean isSourceValid = true;

    private void onWrongProperty(JLabel label, GlobalEnvironment.PropertyType type) {
        infoLabel.setText("Emacs " + String.valueOf(type).toLowerCase() + " directory set failed");
        label.setForeground(Color.red);
    }

    private boolean setServiceParameter(GlobalEnvironment.PropertyType type, JLabel label, EmacsService service, JTextField textField) {
        String name = String.valueOf(type).toLowerCase();
        JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
        fileChooser.setDialogTitle("Select Emacs " + name + " directory");
        fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            String dir = fileChooser.getSelectedFile().getAbsolutePath();
            service.setEmacsParameter(dir);
            textField.setText(dir);
            if (GlobalEnvironment.testProperty(type, dir)) {
                infoLabel.setText("Emacs " + name + " directory successfully set");
                label.setForeground(Color.black);
                return true;
            } else {
                Messages.showErrorDialog(this,
                        "You might have mistaken when set Emacs " + name + " directory.\nTry again.",
                        "Emacs Environment Settings");
                onWrongProperty(label, type);
                return false;
            }
        } else {
            if (GlobalEnvironment.isEmacsPropertyOk(type))
                return true;
            onWrongProperty(label, type);
            return false;
        }
    }

    private void setText (JTextField textField, EmacsService service) {
        String data = service.getEmacsParameter();
        if (data == null)
            data = "";
        textField.setText(data);
        if (service instanceof EmacsHomeService)
            wasHome = data;
        else wasSrc = data;
    }

    private void tryEnableApply() {
        applyButton.setEnabled(isHomeValid && isSourceValid);
    }

    private boolean changed () {
        return (!wasHome.equals(homeTextField.getText()) || !wasSrc.equals(srcTextField.getText()));
    }

    public OptionsForm(Project project) {
        setTitle("Emacs4ij Settings");
        setContentPane(panel1);
        myProject = project;
        myProjectComponent = project.getComponent(MyProjectComponent.class);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        setText(homeTextField, emacsHomeService);
        setText(srcTextField, emacsSourceService);
        if (!EnvironmentInitializer.isGlobalInitialized()) {
            isSourceValid = GlobalEnvironment.testProperty(GlobalEnvironment.PropertyType.SOURCE, emacsSourceService.getEmacsParameter());
            isHomeValid = GlobalEnvironment.testProperty(GlobalEnvironment.PropertyType.HOME, emacsHomeService.getEmacsParameter());
            homeLabel.setForeground(isHomeValid ? Color.black : Color.red);
            srcLabel.setForeground(isSourceValid ? Color.black : Color.red);
        }
        tryEnableApply();

        browseHomeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                isHomeValid = setServiceParameter(GlobalEnvironment.PropertyType.HOME, homeLabel, emacsHomeService, homeTextField);
                tryEnableApply();
            }
        });
        browseSrcButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                isSourceValid = setServiceParameter(GlobalEnvironment.PropertyType.SOURCE, srcLabel, emacsSourceService, srcTextField);
                tryEnableApply();
            }
        });
        quitButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!changed())
                    OptionsForm.this.dispose();
                else {
                    if (applyButton.isEnabled()) {
                        int k = Messages.showOkCancelDialog(myProject, "You have changed settings. Are you sure you don't want to apply them?\nPress Cancel to return.", "Emacs4ij", Messages.getQuestionIcon());
                        if (k == 0) {
                            OptionsForm.this.dispose();
                        }
                    }
                }
            }
        });
        applyButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (changed()) {
                    new Task.Backgroundable(myProject, "Initializing Emacs environment", false) {
                        public void run(@NotNull ProgressIndicator indicator) {
                            indicator.setText("Loading Emacs functions");
                            indicator.setFraction(0.0);
                            EnvironmentInitializer.reset();
                            if (EnvironmentInitializer.initGlobal() && myProjectComponent != null) {
                                wasHome = emacsHomeService.getEmacsParameter();
                                wasSrc = emacsSourceService.getEmacsParameter();
                                myProjectComponent.initEnv();
                            }
                            indicator.setFraction(1.0);
                        }
                    }.queue();
                }
            }
        });
    }
}
