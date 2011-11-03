package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.RoamingType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.ui.Messages;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.emacs4ij.jelisp.Environment;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
@State(
        name="EmacsHomeDirectory",
        storages = @Storage(id="other", file = "$APP_CONFIG$/other.xml"),
        reloadable = true,
        roamingType = RoamingType.DISABLED
    )
public class EmacsHomeService implements PersistentStateComponent<EmacsHomeService> {
    private String myEmacsHome;

    public String getEmacsHome() {
        return myEmacsHome;
    }

    public void setEmacsHome(String emacsHome) {
        myEmacsHome = emacsHome;
    }

    public boolean checkSetEmacsHome () {
        if (myEmacsHome == null || myEmacsHome.equals("")) {
            Messages.showInfoMessage("You should choose Emacs home directory!", "Emacs4ij");
            JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
            fileChooser.setDialogTitle("Select Emacs home directory");
            fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                String emacsHome = fileChooser.getSelectedFile().getAbsolutePath();
                setEmacsHome(emacsHome);
                Environment.ourEmacsPath = emacsHome;
                return true;
            } else {
                Messages.showErrorDialog("You didn't choose Emacs home directory!\nNo command evaluation will be done.", "Emacs4ij");
                return false;
            }
        } else {
            if (Environment.ourEmacsPath.equals(""))
                Environment.ourEmacsPath = myEmacsHome;
            return true;
        }
    }

    @Override
    public EmacsHomeService getState() {
        return this;
    }

    @Override
    public void loadState(EmacsHomeService emacsHomeService) {
        XmlSerializerUtil.copyBean(emacsHomeService, this);
    }
}
