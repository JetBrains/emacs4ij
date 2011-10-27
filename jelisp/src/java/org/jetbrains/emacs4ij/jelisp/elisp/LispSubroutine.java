package org.jetbrains.emacs4ij.jelisp.elisp;

import org.apache.commons.lang.ArrayUtils;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

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
    private static Class[] myBuiltIns = new Class[]{BuiltinsBuffer.class, BuiltinsCore.class, BuiltinsList.class, BuiltinsSymbol.class, BuiltinsCheck.class, BuiltinsMarker.class};
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

    public static Class[] getSubroutineClasses () {
        return (Class[]) ArrayUtils.addAll(myBuiltIns, mySpecialForms);
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

    private static ArgumentsList parseArguments (Method m, Environment environment, List<LObject> args) {
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

    private static int checkParameterizedType (ParameterizedType expectedType, ArgumentsList arguments, List<LObject> args, int argsCounter, int i) {
        Type rawType = expectedType.getRawType();
        Type expectedTypeArguments = expectedType.getActualTypeArguments()[0];
        try {
            if (((Class)rawType).isInstance(args.get(argsCounter))) {
                Type actualTypeArguments = ((ParameterizedType) (Type)args.get(argsCounter).getClass()).getActualTypeArguments()[0];
                if (!expectedTypeArguments.equals(actualTypeArguments)) {
                    throw new WrongTypeArgument(((Class) rawType).getSimpleName()+"<"+((Class)expectedTypeArguments).getSimpleName()+">", args.get(argsCounter).toString());
                }
                arguments.setValue(i, args.get(argsCounter));
                return argsCounter + 1;
            } else {
                if (arguments.isOptional(i))
                    return -1;
                throw new WrongTypeArgument(expectedType.toString(), args.get(argsCounter).toString());
            }
        } catch (IndexOutOfBoundsException e) {
            if (arguments.isOptional(i))
                return -1;
            throw new RuntimeException("wrong arg N check!");
        }
    }

    private static <T> T[] customizeArrayList(Class<T> type, ArrayList array) {
        return (T[]) array.toArray((LObject[]) Array.newInstance(type, 0));
    }

    private static int checkArray (Class expectedType, ArgumentsList arguments, List<LObject> args, int argsCounter, int i) {
        Class componentType = expectedType.getComponentType();
        ArrayList array = new ArrayList();
        while (argsCounter != args.size()) {
            if (!componentType.isInstance(args.get(argsCounter)))
                throw new WrongTypeArgument(componentType.toString(), args.get(argsCounter).toString());
            array.add(args.get(argsCounter));
            ++argsCounter;
        }
        arguments.setValue(i, customizeArrayList(componentType, array));
        return argsCounter;
    }

    private static int checkSingleArgument (Class expectedType, ArgumentsList arguments, List<LObject> args, int argsCounter, int i) {
        try {
            if (!(expectedType.isInstance(args.get(argsCounter)))) {
                if (arguments.isOptional(i))
                    return -1;
                throw new WrongTypeArgument(expectedType.getSimpleName(), args.get(argsCounter).toString());
            }
            arguments.setValue(i, args.get(argsCounter));
            return argsCounter + 1;
        } catch (IndexOutOfBoundsException e) {
            if (arguments.isOptional(i))
                return -1;
            throw new RuntimeException("wrong arg N check!");
        }
    }

    private static void checkArguments (ArgumentsList arguments, List<LObject> args) {
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

    public static LObject evaluate (LispSymbol f, Environment environment, List<LObject> args) {
        Class[] subroutines = getSubroutineClasses();
        for (Class c: subroutines) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                if (annotation.value().equals(f.getName())) {
                    if (!Arrays.asList(mySpecialForms).contains(c)) {
                        for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                            args.set(i, args.get(i).evaluate(environment));
                        }
                    }
                    ArgumentsList arguments = parseArguments(m, environment, args);
                    checkArguments(arguments, args);
                    try {
                        return (LObject) m.invoke(c.newInstance(), arguments.getValues());
                    } catch (IllegalAccessException e) {
                        throw new RuntimeException(e);
                    } catch (InvocationTargetException e) {
                        throw new RuntimeException(e);
                    } catch (InstantiationException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
        throw new RuntimeException("unknown subroutine " + f.getName());
    }
}
