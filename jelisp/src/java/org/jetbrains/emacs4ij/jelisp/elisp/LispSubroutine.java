package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
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
    private static Class[] myBuiltIns = new Class[]{BuiltinsBuffer.class, BuiltinsCore.class, BuiltinsList.class, BuiltinsSymbol.class, BuiltinsCheck.class};
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
            arguments.add(optional);
            }
        }
        arguments.setRequiredSize(nRequiredParameters);
    }

    private static ArgumentsList parseAndCheckArguments (Method m, Environment environment, List<LObject> args) {
        Type[] parametersTypes = m.getGenericParameterTypes();
        Annotation[][] parametersAnnotations = m.getParameterAnnotations();
        if (parametersAnnotations.length != parametersTypes.length) {
            throw new RuntimeException("Parameters types and annotations lengths do not match!");
        }
        ArgumentsList arguments = new ArgumentsList();

        setOptional(arguments, parametersAnnotations, parametersTypes);

        // check arguments number
        int nActual = args.size();
        if (parametersTypes[0].equals(Environment.class)) {
            ++nActual;
            arguments.setValue(0, environment);
        }

        if ((nActual < arguments.getRequiredSize()) || (nActual > parametersTypes.length && !m.isVarArgs()))
            throw new WrongNumberOfArgumentsException(m.getAnnotation(Subroutine.class).value() + nActual);

        // check arguments types
        int argsCounter = 0;

        for (int i=0; i != arguments.getSize(); ++i) {
            Type expectedType = parametersTypes[i];
            if (i==0 && expectedType.equals(Environment.class))
                continue;
            if (ParameterizedType.class.isInstance(expectedType)) {
                Type rawType = ((ParameterizedType) expectedType).getRawType();
                Type expectedTypeArguments = ((ParameterizedType) expectedType).getActualTypeArguments()[0];
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
            } else if (((Class)expectedType).isArray()) {
                Class componentType = ((Class)expectedType).getComponentType();
                ArrayList array = new ArrayList();
                while (argsCounter != args.size() && componentType.isInstance(args.get(argsCounter))) {
                    array.add(args.get(argsCounter));
                    ++argsCounter;
                }

                /*if (parametersTypes.length == 1) {
                    arguments.setValues(array.toArray());
                    break;
                } else */
                //Object[] qqq = array.toArray();
                arguments.setValue(i, array.toArray());

            }

            else {
                if (!(((Class)expectedType).isInstance(args.get(argsCounter)))) {
                    if (arguments.isOptional(i))
                        break;
                    throw new WrongTypeArgument(((Class)expectedType).getSimpleName(), args.get(argsCounter).toString());
                }
                arguments.setValue(i, args.get(argsCounter));
                ++argsCounter;
            }
        }
        return arguments;
    }

    public static LObject evaluate (LispSymbol f, Environment environment, List<LObject> args) {
        Class[] subroutines = null;
        String type = null;
        if (f.is(LispSymbol.FunctionType.SpecialForm)) {
            subroutines = mySpecialForms;
            type = LispSymbol.FunctionType.SpecialForm.getValue();
        }
        if (f.is(LispSymbol.FunctionType.BuiltIn)) {
            subroutines = myBuiltIns;
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
                    ArgumentsList arguments = parseAndCheckArguments(m, environment, args);
                    try {
                        ArrayList<LispInteger> t = new ArrayList<LispInteger>();
                        t.add(new LispInteger(1));
                        t.add(new LispInteger(2));
                        t.add(new LispInteger(3));
                        //return (LObject) m.invoke(null, new Object[]{t.toArray()});
                        Object[] q = arguments.getValues();
                        return (LObject) m.invoke(null, arguments.getValues());
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    } catch (InvocationTargetException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                }
            }
        }

        throw new RuntimeException("unknown " + type + " function " + f.getName());
    }
}
