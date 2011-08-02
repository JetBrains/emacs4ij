package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.*;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:15 PM
 * To change this template use File | Settings | File Templates.
 */
@State(
        name="EmacsHomeDirectory",
        storages = {@Storage(id="default", file = "$WORKSPACE_FILE$")},
        reloadable = true,
        roamingType = RoamingType.DISABLED
    )
public class Emacs4ijPlugin implements ApplicationComponent, PersistentStateComponent<String> {
    private String myEmacsHome;

    public String getEmacsHome() {
        return myEmacsHome;
    }

    public void setEmacsHome(String myEmacsHome) {
        this.myEmacsHome = myEmacsHome;
    }

    public Emacs4ijPlugin() {
        myEmacsHome = "";
    }

    public void initComponent() {
        // TODO: insert component initialization logic here
    }

    public void disposeComponent() {
        // TODO: insert component disposal logic here
    }

    @NotNull
    public String getComponentName() {
        return "org.jetbrains.emacs4ij.Emacs4ijPlugin";
    }

    @Override
    public String getState() {
        return myEmacsHome;
    }

    @Override
    public void loadState(String s) {
        //myEmacsHome = s;
        XmlSerializerUtil.copyBean(s, myEmacsHome);
    }
}
