package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.subroutine.Subroutine;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 12/1/11
 * Time: 5:40 PM
 * To change this template use File | Settings | File Templates.
 */
public class Primitive implements FunctionCell, LispCommand {
    private final String myName;
    private LispObject myDocumentation;
    private final boolean isInteractive;
    private final String myInteractiveString;
    private final Type myType;
    
    private int myMinNumArgs = -1;
    private LispObject myMaxNumArgs = null;

    @Override
    public LispList getInteractiveForm() {
        if (isInteractive())
            return LispList.list(new LispSymbol("interactive"), new LispString(getInteractiveString()));
        return LispList.list();
    }

    public enum Type {BUILTIN, SPECIAL_FORM}

    public Primitive (Subroutine annotation, String documentation, Type type) {
        myName = annotation.value();
        isInteractive = annotation.isCmd();
        myInteractiveString = annotation.interactive();
        myDocumentation = documentation == null ? null : new LispString(documentation);
        myType = type;
        countMinMaxNumArgs();
    }

    @Override
    public String toString() {
        return "#<subr " + myName + ">";
    }

    @Override
    public LispObject getDocumentation() {
        if (myDocumentation == null)
            return LispSymbol.ourNil;        
        if (myDocumentation instanceof LispString)
            return new LispString (((LispString) myDocumentation).getData().replace("usage: (" + myName, "\n(fn"));
        return myDocumentation;
    }

    @Override
    public void setDocumentation(LispObject doc) {
        myDocumentation = doc;
    }

    @Override
    public boolean isInteractive() {
        return isInteractive;
    }

    @Override
    public String getInteractiveString() {
        return isInteractive ? myInteractiveString : null;
    }

    @Override
    public int getNRequiredArguments() {
        return myMinNumArgs;
    }

    public Type getType () {
        return myType;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        return this;
    }

    public void countMinMaxNumArgs() {
        Class[] containers;
        if (myType == Type.SPECIAL_FORM) {
            myMaxNumArgs = new LispSymbol("unevalled");
            containers = LispSubroutine.getSpecialFormsClasses();
        } else {
            containers = LispSubroutine.getBuiltinsClasses();
        }
        for (Class c: containers) {
            Method[] methods = c.getMethods();
            for (Method m: methods) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                if (annotation.value().equals(myName)) {
                    java.lang.reflect.Type[] parametersTypes = m.getGenericParameterTypes();
                    Annotation[][] parametersAnnotations = m.getParameterAnnotations();
                    if (parametersAnnotations.length != parametersTypes.length) {
                        throw new RuntimeException("Parameters types and annotations lengths do not match!");
                    }

                    if (parametersTypes.length == 0) {
                        myMinNumArgs = 0;
                        if (myMaxNumArgs == null)
                            myMaxNumArgs = new LispInteger(0);
                        return;
                    }

                    int correction = (parametersTypes[0].equals(Environment.class)) ? -1 : 0;

                    boolean optional = false;
                    for (int i = 0; i != parametersTypes.length; ++i) {
                        if (!optional) {
                            if (parametersAnnotations[i].length > 0 && parametersAnnotations[i][0] instanceof Optional) {
                                optional = true;
                                myMinNumArgs = i + correction;
                            }
                        }
                        if (((Class)parametersTypes[i]).isArray()) {
                            if (myMaxNumArgs == null)
                                myMaxNumArgs = new LispSymbol("many");
                        }
                    }
                    if (myMinNumArgs == -1)
                        myMinNumArgs = parametersTypes.length + correction;
                    if (myMaxNumArgs == null)
                        myMaxNumArgs = new LispInteger(parametersTypes.length + correction);
                    
                    return;
                }
            }
        }
        throw new RuntimeException("Cannot count min and max number of arguments for primitive " + myName);
    }

    public LispObject getMaxNumArgs() {
        return myMaxNumArgs;
    }

}
