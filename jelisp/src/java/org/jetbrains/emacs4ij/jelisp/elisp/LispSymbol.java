package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp symbol = variable name, function name, constant name, special form name, etc
 */
public class LispSymbol extends LispAtom {
    public static final LispSymbol ourNil = new LispSymbol("nil");
    public static final LispSymbol ourT = new LispSymbol("t");
    public static final LispSymbol ourVoid = new LispSymbol("void");
    public static final LispSymbol ourBufferLocalVariable = new LispSymbol("");
    public static final LispSymbol ourUserOption = new LispSymbol("");

    private String myName = null;
    private LObject myValue = null; //ourVoid;
    private LispObject myFunction = null;
    private boolean isInteractive = false;
    private String myInteractiveString;

    private HashMap<LispSymbol, LispObject> myProperties = new HashMap<LispSymbol, LispObject>();

    public LispSymbol(String myName) {
        this.myName = myName;
    }

    public static LispSymbol newSubroutine (String myName, boolean isCommand, String interactiveString) {
        LispSymbol subroutine = new LispSymbol(myName);
        subroutine.myFunction = new LispString("#<subr " + myName + ">");
        subroutine.isInteractive = isCommand;
        subroutine.myInteractiveString = interactiveString;
        return subroutine;
    }

    public LispSymbol (String myName, LObject value) {
        this.myName = myName;
        myValue = value;
    }

    public String getName() {
        return myName;
    }

    public LObject getValue() {
        return myValue;
    }

    public void setValue(LObject myValue) {
        this.myValue = myValue;
    }

    public FunctionCell getFunctionCell() {
        if (myFunction == null)
            return null;
        if (isCustom())
            return new FunctionCell(toString(), FunctionCell.Type.CustomFunction);
        if (isSubroutine()) {
            if (isBuiltIn())
                return new FunctionCell(toString(), FunctionCell.Type.BuiltIn);
            return new FunctionCell(toString(), FunctionCell.Type.SpecialForm);
        }
        throw new RuntimeException("unknown function type: " + myName);
    }

    public void castToLambda (Environment environment) {
        if (myFunction instanceof LispList) {
            myFunction = new Lambda((LispList) myFunction, environment);
            environment.updateFunction(this);
        }
    }

    public LObject getFunction () {
        return myFunction;
    }

    public void setFunction(LispObject myFunction) {
        this.myFunction = myFunction;
    }

    @Override
    public String toString() {
        if (myFunction == null)
            return myName;
        if (isSubroutine())
            return "#<subr " + myName + '>';
        return myFunction.toString();
    }

    public boolean isSubroutine () {
        return (myFunction instanceof LispString);
    }

    public boolean isBuiltIn () {
        if (!isSubroutine())
            return false;
        for (Class c: LispSubroutine.getSpecialFormsClasses()) {
            for (Method m: c.getMethods()) {
                Subroutine annotation = m.getAnnotation(Subroutine.class);
                if (annotation == null)
                    continue;
                if (annotation.value().equals(myName)) {
                    return false;
                }
            }
        }
        return true;
    }

    public boolean isCustom() {
        return ((myFunction instanceof LispList) || (myFunction instanceof Lambda));
    }

    public boolean isFunction() {
        return myFunction != null;
    }

    public boolean isInteractive () {
        return isInteractive;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;

        if (o.getClass().equals(LispList.class) && ((LispList)o).getData().isEmpty() && this.equals(ourNil))
            return true;
        if (o.getClass() != getClass())
            return false;

        LispSymbol that = (LispSymbol) o;

        return !(myName != null ? !myName.equals(that.myName) : that.myName != null);

    }

    @Override
    public int hashCode() {
        int result = myName != null ? myName.hashCode() : 0;
        result = 31 * result + (myValue != null ? myValue.hashCode() : 0);
        result = 31 * result + (myFunction != null ? myFunction.hashCode() : 0);
        result = 31 * result + (myProperties != null ? myProperties.hashCode() : 0);
        return result;
    }

    public boolean hasValue () {
        return myValue != null;
    }

    @Override
    /**
     * takes Environment
     */
    public LObject evaluate(Environment environment) {
        if (equals(ourNil) || equals(ourT) || equals(ourVoid))
            return this;
        if (myName.equals("default-directory"))
            return environment.getDefaultDirectory();
        if (hasValue()) {
            return getValue();
        }
        LispSymbol symbol = environment.find(myName);
        if (symbol == null || (!symbol.hasValue()))
            throw new VoidVariableException(myName);
        return symbol.getValue();
    }

    public LObject evaluateFunction (Environment environment, List<LObject> args) {
        if (isSubroutine())
            return LispSubroutine.evaluate(this, environment, args);

        if (!environment.areArgumentsEvaluated()) {
            for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                args.set(i, args.get(i).evaluate(environment));
            }
        } else {
            environment.setArgumentsEvaluated(false);
        }

        if (!(myFunction instanceof Lambda)) {
            myFunction = new Lambda((LispList)myFunction, environment);
        }
        return ((Lambda)myFunction).evaluate(environment, args);
    }

    public LispObject getPropertyList() {
        LispList pList = new LispList();
        for (LispSymbol key: myProperties.keySet())
            pList.add(new LispList(key, myProperties.get(key)));
        return pList;
    }

    public LispObject getProperty (String pName) {
        return getProperty(new LispSymbol(pName));
    }

    public LispObject getProperty(LispSymbol pName) {
        if (myProperties.containsKey(pName))
            return myProperties.get(pName);
        return LispSymbol.ourNil;
    }

    public void setProperty(LispSymbol key, LispObject value) {
        myProperties.put(key, value);
    }

    public void setProperty(String keyName, LispObject value) {
        myProperties.put(new LispSymbol(keyName), value);
    }

    public LispObject getVariableDocumentation () {
        return getProperty("variable-documentation");
    }

    public void setVariableDocumentation (LispString value) {
        setProperty("variable-documentation", value);
    }

    public LispObject getCustomFunctionDocumentation () {
        if (myFunction instanceof Lambda)
            return ((Lambda) myFunction).getDocString();
        throw new RuntimeException("invalid function call");
    }

    public String getInteractiveString () {
        if (!isFunction())
            return null;
        if (myFunction instanceof Lambda)
            return ((Lambda) myFunction).getInteractiveString();
        if (isSubroutine())
            return myInteractiveString;
        return null;
    }



}
