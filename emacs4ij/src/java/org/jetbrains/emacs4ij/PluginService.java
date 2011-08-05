package org.jetbrains.emacs4ij;

import com.intellij.ui.EditorTextField;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 6:10 PM
 * To change this template use File | Settings | File Templates.
 */
public class PluginService {
    private EditorTextField myInput = new EditorTextField();

    public EditorTextField getInput() {
        return myInput;
    }
}
