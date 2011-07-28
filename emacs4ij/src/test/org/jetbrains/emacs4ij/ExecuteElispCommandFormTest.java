package org.jetbrains.emacs4ij;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/20/11
 * Time: 11:36 AM
 * To change this template use File | Settings | File Templates.
 */
public class ExecuteElispCommandFormTest {

    public static void main (String[] args) {
        ExecuteElispCommandForm f = new ExecuteElispCommandForm();
        f.setCommandInteractive("aa: ");
        f.setVisible(true);
    }
}
