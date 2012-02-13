package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.RoamingType;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/1/11
 * Time: 2:32 PM
 * To change this template use File | Settings | File Templates.
 */
@State(
        name="EmacsSource",
        storages = @Storage(id="other", file = "$APP_CONFIG$/other.xml"),
        reloadable = true,
        roamingType = RoamingType.DISABLED
    )
public class EmacsSourceService extends EmacsService implements PersistentStateComponent<EmacsSourceService>  {
    /*public boolean checkSetEmacsSource  () {
        if (GlobalEnvironment.ourEmacsSource.equals(""))
            GlobalEnvironment.ourEmacsSource = checkSetEmacsParameter("source");
        return !GlobalEnvironment.ourEmacsSource.equals("");
    }

    public boolean resetEmacsSource () {
        GlobalEnvironment.ourEmacsSource = reset("source");
        return !GlobalEnvironment.ourEmacsSource.equals("");
    }     */

    @Override
    public EmacsSourceService getState() {
        return this;
    }

    @Override
    public void loadState(EmacsSourceService emacsSourceService) {
        XmlSerializerUtil.copyBean(emacsSourceService, this);
    }

    @Override
    protected boolean isParameterSet() {
        if (myEmacsParameter != null && !myEmacsParameter.equals("")) {
            GlobalEnvironment.setEmacsSource(myEmacsParameter);
            return true;
        }
        return false;
    }
}
