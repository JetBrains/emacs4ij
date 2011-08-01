package org.jetbrains.emacs4ij;

import com.intellij.openapi.components.*;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/19/11
 * Time: 7:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class Emacs4ijPlugin implements ApplicationComponent {

    public Emacs4ijPlugin() {
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
}
