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

    private void setServiceParameter(GlobalEnvironment.PropertyType type, JLabel label, EmacsService service, JTextField textField) {
        String name = type == GlobalEnvironment.PropertyType.HOME ? "home" : "source";
        while (true) {
            JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
            fileChooser.setDialogTitle("Select Emacs " + name + " directory");
            fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                String dir = fileChooser.getSelectedFile().getAbsolutePath();
                service.setEmacsParameter(dir);
                if (service instanceof EmacsSourceService)
                    GlobalEnvironment.setEmacsSource(dir);
                else GlobalEnvironment.setEmacsHome(dir);

                boolean test = (service instanceof EmacsHomeService) ? GlobalEnvironment.INSTANCE.testEmacsHome()
                        : GlobalEnvironment.INSTANCE.testEmacsSource();
                if (test) {
                    textField.setText(dir);
                    infoLabel.setText("Emacs source directory successfully set");
                    label.setForeground(Color.black);
                    break;
                }
            } else {
                if (GlobalEnvironment.isEmacsPropertyOk(type))
                    return;
                infoLabel.setText("Emacs source directory set failed");
                label.setForeground(Color.red);
                return;
            }
        }
    }

    private void setText (JTextField textField, EmacsService service) {
        String data = service.getEmacsParameter();
        if (data == null)
            data = null;
        textField.setText(data);
        if (service instanceof EmacsHomeService)
            wasHome = data;
        else wasSrc = data;
    }

    public OptionsForm(Project project) {
        setTitle("Emacs4ij settings");
        setContentPane(panel1);
        myProject = project;
        myProjectComponent = project.getComponent(MyProjectComponent.class);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        emacsHomeService = ServiceManager.getService(EmacsHomeService.class);
        emacsSourceService = ServiceManager.getService(EmacsSourceService.class);
        setText(homeTextField, emacsHomeService);
        setText(srcTextField, emacsSourceService);

        browseHomeButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setServiceParameter(GlobalEnvironment.PropertyType.HOME, homeLabel, emacsHomeService, homeTextField);
            }
        });
        browseSrcButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setServiceParameter(GlobalEnvironment.PropertyType.SRC, srcLabel, emacsSourceService, srcTextField);
            }
        });
        quitButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (wasHome.equals(homeTextField.getText()) && wasSrc.equals(srcTextField.getText()))
                    OptionsForm.this.dispose();
                else {
                    int k = Messages.showOkCancelDialog(myProject, "You have changed settings. Are you sure you don't want to apply them?\nPress Cancel to return.", "Emacs4ij", null);
                    if (k == 0) {
                        OptionsForm.this.dispose();
                    }
                }
            }
        });
        applyButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                boolean changed = (wasHome.equals(homeTextField.getText()) && wasSrc.equals(srcTextField.getText()));
                OptionsForm.this.dispose();
                if (changed)
                    new Task.Backgroundable(myProject, "Initializing Emacs environment", false) {
                        public void run(@NotNull ProgressIndicator indicator) {
                            indicator.setText("Loading Emacs functions");
                            indicator.setFraction(0.0);
                            EnvironmentInitializer.reset();
                            if (EnvironmentInitializer.initGlobal() && myProjectComponent != null) {
                                myProjectComponent.initEnv();
                            }
                            indicator.setFraction(1.0);
                        }
                    }.queue();

            }
        });
    }
}
