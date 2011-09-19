package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.EnvironmentException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;

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
public abstract class LispSubroutine {
    private static Class[] builtins = new Class[]{BuiltinsBuffer.class, BuiltinsCore.class, BuiltinsList.class, BuiltinsSymbol.class, BuiltinsCheck.class};
    private static Class[] specialForms = new Class[] {SpecialForms.class};

    private LispSubroutine() {}

    public static LispObject evaluate(LispSymbol f, Environment environment, List<LObject> args) {
        Class[] subroutines = null;
        String type = null;
        if (f.is(LispSymbol.FunctionType.SpecialForm)) {
            subroutines = specialForms;
            type = LispSymbol.FunctionType.SpecialForm.getValue();
        }
        if (f.is(LispSymbol.FunctionType.BuiltIn)) {
            subroutines = builtins;
            type = LispSymbol.FunctionType.BuiltIn.getValue();
            for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                args.set(i, args.get(i).evaluate(environment));
            }
        }
        if (subroutines == null)
            throw new RuntimeException("invalid usage of symbol evaluation: " + f.getName());

        for (Class c: subroutines) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                if (annotation.value().equals(f.getName()))
                    try {
                        if (annotation.exact() != -1) {
                            if (args.size() != annotation.exact())
                                throw new WrongNumberOfArgumentsException(f.getName());
                        } else {
                            if ((annotation.min() != -1 && args.size() < annotation.min()) || (annotation.max() != -1 && args.size() > annotation.max()))
                                throw new WrongNumberOfArgumentsException(f.getName());
                        }
                        return (LispObject) m.invoke(null, environment, args);
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        throw new RuntimeException(e.getMessage());
                    } catch (InvocationTargetException e) {
                        if (e.getTargetException() instanceof LispException)
                            throw (LispException) e.getTargetException();
                        if (e.getTargetException() instanceof EnvironmentException)
                            throw (EnvironmentException) e.getTargetException();
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        throw new RuntimeException(e.getMessage());
                    }

            }
        }

        throw new RuntimeException("unknown " + type + " function " + f.getName());
    }
}
