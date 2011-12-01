package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.RoamingType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.ui.Messages;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

import javax.swing.*;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/1/11
 * Time: 2:32 PM
 * To change this template use File | Settings | File Templates.
 */
@State(
        name="EmacsSourceDirectory",
        storages = @Storage(id="other", file = "$APP_CONFIG$/other.xml"),
        reloadable = true,
        roamingType = RoamingType.DISABLED
    )
public class EmacsSourceService implements PersistentStateComponent<EmacsSourceService> {
     private String myEmacsSource;

    public String getEmacsSource() {
        return myEmacsSource;
    }

    public void setEmacsSource(String emacsSource) {
        myEmacsSource = emacsSource;
    }

    public boolean checkSetEmacsSource  () {
        if (myEmacsSource == null || myEmacsSource.equals("")) {
            Messages.showInfoMessage("You should choose Emacs source directory (with .el sources)!", "Emacs4ij");
            JFileChooser fileChooser = new JFileChooser(System.getProperty("user.home"));
            fileChooser.setDialogTitle("Select Emacs source directory");
            fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            if (fileChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                String emacsSource = fileChooser.getSelectedFile().getAbsolutePath();
                setEmacsSource(emacsSource);
                GlobalEnvironment.ourEmacsSource = emacsSource;
                return true;
            } else {
                Messages.showInfoMessage("You didn't choose Emacs source directory (with .el sources)!\nNo command evaluation will be done.", "Emacs4ij");
                return false;
            }
        } else {
            if (GlobalEnvironment.ourEmacsSource.equals("")) {
                GlobalEnvironment.ourEmacsSource = myEmacsSource;
            }
            return true;
        }
    }

    @Override
    public EmacsSourceService getState() {
        return this;
    }

    @Override
    public void loadState(EmacsSourceService emacsSourceService) {
        XmlSerializerUtil.copyBean(emacsSourceService, this);
    }
}
