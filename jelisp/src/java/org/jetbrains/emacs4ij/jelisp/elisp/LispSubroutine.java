package org.jetbrains.emacs4ij.jelisp.elisp;

import org.apache.commons.lang.ArrayUtils;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.LispThrow;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;
import org.jetbrains.emacs4ij.jelisp.subroutine.*;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
/**
* Created by IntelliJ IDEA.
* User: Ekaterina.Polishchuk
* Date: 8/2/11
* Time: 5:14 PM
* To change this template use File | Settings | File Templates.
*/
public abstract class LispSubroutine {
    private static Class[] myBuiltIns = new Class[] {BuiltinArithmetic.class,
                                                    BuiltinPredicates.class,
                                                    BuiltinsBuffer.class,
                                                    BuiltinsCore.class,
                                                    BuiltinsFrame.class,
                                                    BuiltinsKey.class,
                                                    BuiltinsList.class,
                                                    BuiltinsMarker.class,
                                                    BuiltinsSequence.class,
                                                    BuiltinsString.class,
                                                    BuiltinsSymbol.class,
                                                    BuiltinsVector.class};
    private static Class[] mySpecialForms = new Class[] {SpecialForms.class};

    private LispSubroutine() {}

    private static boolean isOptional (Annotation[] parameterAnnotations) {
        for (Annotation a: parameterAnnotations) {
            if (a instanceof Optional) {
                return true;
            }
        }
        return false;
    }

    public static Class[] getSubroutineContainers () {
        return (Class[]) ArrayUtils.addAll(myBuiltIns, mySpecialForms);
    }

    public static Class[] getBuiltinsClasses() {
        return myBuiltIns;
    }

    public static Class[] getSpecialFormsClasses () {
        return mySpecialForms;
    }

    private static void setOptional(ArgumentsList arguments, Annotation[][] parametersAnnotations, Type[] parametersTypes) {
        boolean optional = false;
        int nRequiredParameters = parametersAnnotations.length;
        for (int i=0; i!=parametersAnnotations.length; ++i) {
            if (!optional) {
                if (isOptional(parametersAnnotations[i])) {
                    nRequiredParameters = i;
                    optional = true;
                }
            }
            arguments.add(optional, parametersTypes[i]);
        }
        arguments.setRequiredSize(nRequiredParameters);
    }

    private static ArgumentsList parseArguments (Method m, Environment environment, List<LispObject> args) {
        Type[] parametersTypes = m.getGenericParameterTypes();
        Annotation[][] parametersAnnotations = m.getParameterAnnotations();
        if (parametersAnnotations.length != parametersTypes.length) {
            throw new RuntimeException("Parameters types and annotations lengths do not match!");
        }
        ArgumentsList arguments = new ArgumentsList();
        setOptional(arguments, parametersAnnotations, parametersTypes);

        int nActual = args.size();
        if (parametersTypes.length != 0) {
            if (parametersTypes[0].equals(Environment.class)) {
                ++nActual;
                arguments.setValue(0, environment);
            }
        }

        if ((nActual < arguments.getRequiredSize()) || (nActual > parametersTypes.length && !m.isVarArgs()))
            throw new WrongNumberOfArgumentsException(m.getAnnotation(Subroutine.class).value(), nActual);

        return arguments;
    }

    private static int checkParameterizedType (ParameterizedType expectedType, ArgumentsList arguments, List<LispObject> args, int argsCounter, int i) {
        Type rawType = expectedType.getRawType();
        Type expectedTypeArguments = expectedType.getActualTypeArguments()[0];
        try {
            if (((Class)rawType).isInstance(args.get(argsCounter))) {
                Type actualTypeArguments = ((ParameterizedType) (Type)args.get(argsCounter).getClass()).getActualTypeArguments()[0];
                if (!expectedTypeArguments.equals(actualTypeArguments)) {
                    throw new WrongTypeArgumentException(((Class) rawType).getSimpleName()+"<"+((Class)expectedTypeArguments).getSimpleName()+">", args.get(argsCounter));
                }
                arguments.setValue(i, args.get(argsCounter));
                return argsCounter + 1;
            } else {
                if (arguments.isOptional(i))
                    return -1;
                throw new WrongTypeArgumentException(expectedType.toString(), args.get(argsCounter));
            }
        } catch (IndexOutOfBoundsException e) {
            if (arguments.isOptional(i))
                return -1;
            throw new RuntimeException("wrong arg N check!");
        }
    }

    private static <T> T[] customizeArrayList(Class<T> type, ArrayList array) {
        return (T[]) array.toArray((LispObject[]) Array.newInstance(type, 0));
    }

    private static int checkArray (Class expectedType, ArgumentsList arguments, List<LispObject> args, int argsCounter, int i) {
        Class componentType = expectedType.getComponentType();
        ArrayList array = new ArrayList();
        while (argsCounter != args.size()) {
            if (!componentType.isInstance(args.get(argsCounter)))
                throw new WrongTypeArgumentException(componentType.toString(), args.get(argsCounter));
            array.add(args.get(argsCounter));
            ++argsCounter;
        }
        arguments.setValue(i, customizeArrayList(componentType, array));
        return argsCounter;
    }

    private static int checkSingleArgument (Class expectedType, ArgumentsList arguments, List<LispObject> args, int argsCounter, int i) {
        try {
            if (!(expectedType.isInstance(args.get(argsCounter)))) {
                if (expectedType.equals(LispList.class) && args.get(argsCounter).equals(LispSymbol.ourNil)) {
                    arguments.setValue(i, LispList.list());
                    return argsCounter + 1;
                }
                if (expectedType.equals(LispSymbol.class) && args.get(argsCounter).equals(LispSymbol.ourNil)) {
                    arguments.setValue(i, LispSymbol.ourNil);
                    return argsCounter + 1;
                }
                if (arguments.isOptional(i))
                    return -1;

                throw new WrongTypeArgumentException(expectedType.getSimpleName(), args.get(argsCounter));
            }
            arguments.setValue(i, args.get(argsCounter));
            return argsCounter + 1;
        } catch (IndexOutOfBoundsException e) {
            if (arguments.isOptional(i))
                return -1;
            throw new RuntimeException("wrong arg N check!");
        }
    }

    private static void checkArguments (ArgumentsList arguments, List<LispObject> args) {
        int argsCounter = 0;
        for (int i=0; i != arguments.getSize(); ++i) {
            Type expectedType = arguments.getType(i);
            if (i==0 && expectedType.equals(Environment.class))
                continue;
            if (ParameterizedType.class.isInstance(expectedType)) {
                argsCounter = checkParameterizedType((ParameterizedType) expectedType, arguments, args, argsCounter, i);
            } else if (((Class)expectedType).isArray()) {
                argsCounter = checkArray((Class) expectedType, arguments, args, argsCounter, i);
            }
            else {
                argsCounter = checkSingleArgument((Class) expectedType, arguments, args, argsCounter, i);
            }
            if (argsCounter == -1)
                break;
        }
    }

    private static Throwable getCause (Throwable e) {
        if (e.getCause() == null)
            return e;
        return getCause(e.getCause());
    }

    public static LispObject evaluate (LispSymbol f, Environment environment, List<LispObject> args) {
        for (Class c: getSubroutineContainers()) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                if (annotation.value().equals(f.getName())) {
                    if (!Arrays.asList(mySpecialForms).contains(c)) {
                        if (!environment.areArgumentsEvaluated()) {
                            for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                                args.set(i, args.get(i).evaluate(environment));
                            }
                        } else {
                            environment.setArgumentsEvaluated(false);
                        }
                    }
                    ArgumentsList arguments = parseArguments(m, environment, args);
                    checkArguments(arguments, args);
                    try {
                        return (LispObject) m.invoke(null, arguments.getValues());
                    } catch (IllegalAccessException e) {
                        System.err.println(e.getCause().getMessage());
                        throw new RuntimeException(e.getCause());
                    } catch (InvocationTargetException e) {
                        if (getCause(e) instanceof LispThrow)
                            throw (LispThrow)getCause(e);
                        System.err.println(e.getCause().getMessage());
                        throw new RuntimeException(e.getCause());
                    }
                }
            }
        }
        throw new RuntimeException("unknown subroutine " + f.getName());
    }
}
