package org.jetbrains.emacs4ij.jelisp.elisp;


import org.jetbrains.emacs4ij.jelisp.Environment;

/**
 * Created by IntelliJ IDEA.
 * User: ekaterina.polishchuk
 * Date: 7/8/11
 * Time: 1:28 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract interface LispObject {
    LispObject evaluate(Environment environment);

//    @Override
//    public LispObject invokeMethod(String methodName, Class[] parameterTypes, Object... methodParameters) {
//        try {
//            Method m = this.getClass().getMethod(methodName, parameterTypes);
//            LispObject result = (LispObject) m.invoke(this, methodParameters);
//            return result == null ? this : result;
//        } catch (Exception e) {
//            return this;
//        }
//    }
}
