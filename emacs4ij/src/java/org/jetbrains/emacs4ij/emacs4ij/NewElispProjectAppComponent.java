package org.jetbrains.emacs4ij.emacs4ij;

import com.intellij.openapi.components.ApplicationComponent;
import com.intellij.openapi.ui.Messages;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 4:22 PM
 * To change this template use File | Settings | File Templates.
 */
public class NewElispProjectAppComponent implements ApplicationComponent {
    public NewElispProjectAppComponent() {
    }

    public void initComponent() {
        // TODO: insert component initialization logic here
    }

    public void disposeComponent() {
        // TODO: insert component disposal logic here
    }

    @NotNull
    public String getComponentName() {
        return "NewElispProjectAppComponent";
    }

    public void createNewProject () {
         Messages.showMessageDialog(
                        "Hello World!",
                        "Sample",
                        Messages.getInformationIcon()
                    );
    }
}
