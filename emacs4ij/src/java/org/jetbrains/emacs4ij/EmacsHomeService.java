package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.RoamingType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;

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

    @Override
    public EmacsHomeService getState() {
        return this;
    }

    @Override
    public void loadState(EmacsHomeService emacsHomeService) {
        XmlSerializerUtil.copyBean(emacsHomeService, this);
    }
}
