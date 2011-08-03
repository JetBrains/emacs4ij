package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:14 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class LispBuiltInFunction {
    private static Class[] builtins = new Class[]{BuiltinsCore.class, BuiltinsList.class, BuiltinsSymbol.class, BuiltinsTypeCheck.class};

    private LispBuiltInFunction() {}

    public static LispObject evaluate(String name, Environment environment, List<LispObject> args) {
        for (Class c: builtins) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Builtin annotation = m.getAnnotation(Builtin.class);
                if (annotation == null)
                    continue;
                if (annotation.value().equals(name))
                    try {
                        return (LispObject) m.invoke(null, environment, args);
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        throw new RuntimeException(e.getMessage());
                    } catch (InvocationTargetException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        throw new RuntimeException(e.getMessage());
                    }

            }
        }
        throw new RuntimeException("unknown builtin function " + name);
    }
}
