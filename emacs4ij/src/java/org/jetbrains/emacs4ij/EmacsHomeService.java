package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.RoamingType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/3/11
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */
@State(
        name="EmacsHome",
        storages = @Storage(id="other", file = "$APP_CONFIG$/other.xml"),
        reloadable = true,
        roamingType = RoamingType.DISABLED
    )

public class EmacsHomeService extends EmacsService implements PersistentStateComponent<EmacsHomeService> {

    public boolean checkSetEmacsHome () {
        if (GlobalEnvironment.getEmacsHome().equals(""))
            GlobalEnvironment.setEmacsHome(checkSetEmacsParameter("home"));
        return !GlobalEnvironment.getEmacsHome().equals("");
    }

    public boolean resetEmacsHome () {
        GlobalEnvironment.setEmacsHome(reset("home"));
        return !GlobalEnvironment.getEmacsHome().equals("");
    }

    @Override
    public EmacsHomeService getState() {
        return this;
    }

    @Override
    public void loadState(EmacsHomeService emacsHomeService) {
        XmlSerializerUtil.copyBean(emacsHomeService, this);
    }
    
    @Override
    protected boolean isParameterSet() {
        if (myEmacsParameter != null && !myEmacsParameter.equals("")) {
            GlobalEnvironment.setEmacsHome(myEmacsParameter);
            return true;
        }
        return false;
    }
}
