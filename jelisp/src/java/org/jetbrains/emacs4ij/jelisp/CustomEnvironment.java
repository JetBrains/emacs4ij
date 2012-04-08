package org.jetbrains.emacs4ij.jelisp;

import org.jetbrains.annotations.NotNull;


/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 5:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class CustomEnvironment extends Environment {
    public CustomEnvironment(@NotNull final Environment outerEnv) {
        myOuterEnv = outerEnv;
    }
}
