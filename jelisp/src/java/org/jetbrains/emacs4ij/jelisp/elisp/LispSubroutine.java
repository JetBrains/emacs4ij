package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
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

    private static int optionalParameters (Annotation[][] parameterAnnotations) {
        return 0;
    }

    private static String expectedType (Type[] parameterType) {
        return "";
    }

    private static String gotType (List<LObject> args) {
        return "";
    }

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
                if (annotation.value().equals(f.getName())) {

                    Type[] parameterType = m.getGenericParameterTypes();
                    Annotation[][] parameterAnnotations = m.getParameterAnnotations();
                    if ((args.size() < optionalParameters(parameterAnnotations)) || (args.size() > parameterType.length))
                        throw new WrongNumberOfArgumentsException(f.getName() + args.size());
                    for (int i=0; i!=args.size(); ++i) {

                        if (parameterType[i] instanceof ParameterizedType) {
                            Type actualType = ((ParameterizedType) parameterType[0]).getActualTypeArguments()[0];
                            Type rawType = ((ParameterizedType) parameterType[0]).getRawType();
                            if (rawType.getClass().equals(args.get(0).getClass())) {         //or is instance?
                                for


                            } else {
                                throw new WrongTypeArgument(expectedType(parameterType), gotType(args));
                            }

                        }
                    }



                    try {
                        return (LispObject) m.invoke(null, environment, args);
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    } catch (InvocationTargetException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }

                }
               /*     try {
                        Type[] parameterTypes = m.getGenericParameterTypes();

                        //if (args.size() != )
                        for (Type parameterType: parameterTypes) {

                            if (((Class)parameterType).isInstance(q)) {
                                    System.out.println("yo");

                                }

                            if (parameterType instanceof ParameterizedType) {
                                Type inner = ((ParameterizedType) parameterType).getActualTypeArguments()[0];
                                if (((Class)inner).isInstance(q)) {
                                    System.out.println("yo");

                                }


                            }
                        }


                        Annotation[][] aa = m.getParameterAnnotations();


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
                    }  */

            }
        }

        throw new RuntimeException("unknown " + type + " function " + f.getName());
    }
}
