package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.ArrayList;
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

    public static Class[] getBuiltInsClasses () {
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


    private static <T> T[] customizeArrayList(Class<T> type, ArrayList array) {
        return (T[]) array.toArray((LObject[]) Array.newInstance(type, 0));
    }

    private static ArgumentsList parseArguments (Method m, Environment environment, List<LObject> args) {
        Type[] parametersTypes = m.getGenericParameterTypes();
        Annotation[][] parametersAnnotations = m.getParameterAnnotations();
        if (parametersAnnotations.length != parametersTypes.length) {
            throw new RuntimeException("Parameters types and annotations lengths do not match!");
        }
        ArgumentsList arguments = new ArgumentsList();

        setOptional(arguments, parametersAnnotations, parametersTypes);

        // check arguments number
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

    private static void checkArguments (ArgumentsList arguments, List<LObject> args, boolean checkTypes) {
        int argsCounter = 0;

        for (int i=0; i != arguments.getSize(); ++i) {
            Type expectedType = arguments.getType(i);
            if (i==0 && expectedType.equals(Environment.class))
                continue;
            if (ParameterizedType.class.isInstance(expectedType)) { //List<>
                Type rawType = ((ParameterizedType) expectedType).getRawType();
                Type expectedTypeArguments = ((ParameterizedType) expectedType).getActualTypeArguments()[0];
                try {
                if (((Class)rawType).isInstance(args.get(argsCounter))) {
                    Type actualTypeArguments = ((ParameterizedType) (Type)args.get(argsCounter).getClass()).getActualTypeArguments()[0];
                    if (!expectedTypeArguments.equals(actualTypeArguments)) {
                        throw new WrongTypeArgument(((Class) rawType).getSimpleName()+"<"+((Class)expectedTypeArguments).getSimpleName()+">", args.get(argsCounter).toString());
                    }
                    arguments.setValue(i, args.get(argsCounter));
                    ++argsCounter;
                } else {
                    if (arguments.isOptional(i))
                        break;
                    throw new WrongTypeArgument(expectedType.toString(), args.get(argsCounter).toString());
                }
                } catch (IndexOutOfBoundsException e) {
                    if (!arguments.isOptional(i))
                        throw new RuntimeException("wrong arg N check!");
                }
            } else if (((Class)expectedType).isArray()) { //Object...
                Class componentType = ((Class)expectedType).getComponentType();
                ArrayList array = new ArrayList();
                while (argsCounter != args.size()) {
                    if (!componentType.isInstance(args.get(argsCounter)))
                        throw new WrongTypeArgument(componentType.toString(), args.get(argsCounter).toString());
                    array.add(args.get(argsCounter));
                    ++argsCounter;
                }
                arguments.setValue(i, customizeArrayList(componentType, array));
            }
            else { // one by one
                try {
                if (!(((Class)expectedType).isInstance(args.get(argsCounter)))) {
                    if (arguments.isOptional(i))
                        continue;
                    throw new WrongTypeArgument(((Class)expectedType).getSimpleName(), args.get(argsCounter).toString());
                }
                arguments.setValue(i, args.get(argsCounter));
                ++argsCounter;
                } catch (IndexOutOfBoundsException e) {
                    if (!arguments.isOptional(i))
                        throw new RuntimeException("wrong arg N check!");
                }
            }
        }
    }

    public static LObject evaluate (LispSymbol f, Environment environment, List<LObject> args) {
        Class[] subroutines = null;
        LispSymbol.FunctionType type = LispSymbol.FunctionType.SpecialForm;
        if (f.is(LispSymbol.FunctionType.SpecialForm)) {
            subroutines = mySpecialForms;
        }
        if (f.is(LispSymbol.FunctionType.BuiltIn)) {
            subroutines = myBuiltIns;
            type = LispSymbol.FunctionType.BuiltIn;
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
                    ArgumentsList arguments = parseArguments(m, environment, args);
                    checkArguments(arguments, args, (type.equals(LispSymbol.FunctionType.BuiltIn)));
                    try {
                        return (LObject) m.invoke(null, arguments.getValues());
                    } catch (IllegalAccessException e) {
                        throw new RuntimeException(e);
                    } catch (InvocationTargetException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
        throw new RuntimeException("unknown " + type.getValue() + " " + f.getName());
    }
}
