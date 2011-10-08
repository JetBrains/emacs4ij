package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/4/11
 * Time: 10:50 AM
 * To change this template use File | Settings | File Templates.
 */
public interface LObject {
   // LispString toLispString();

    LObject evaluate(Environment environment);

    LObject invokeMethod(String methodName, Class[] parameterTypes, Object... methodParameters);
}
