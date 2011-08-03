package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.HashMap;

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

    public enum FunctionType {SpecialForm ("special-form"), BuiltIn ("built-in"), Custom ("custom");
        private final String myValue;
        FunctionType (String value) {
            myValue = value;
        }
        public String getValue() {
            return myValue;
        }
    }

    private String myName = null;
    private LispObject myValue = ourVoid;
    private LispObject myFunction = ourVoid;
    private HashMap<LispSymbol, LispObject> myProperties = new HashMap<LispSymbol, LispObject>();


    public LispSymbol(String myName) {
        this.myName = myName;
    }

    public LispSymbol (String myName, FunctionType functionType) {
        this.myName = myName;
        if (functionType.equals(FunctionType.BuiltIn) || functionType.equals(FunctionType.SpecialForm)) {
            myFunction = new LispString("<#subr " + myName + ">");
            setProperty("function-type", new LispString(functionType.getValue()));
            return;
        }
        throw new RuntimeException("Invalid initialization of function " + myName + ", type " + functionType.getValue());
    }

    public LispSymbol (String myName, LispObject value) {
        this.myName = myName;
        myValue = value;
    }

    public String getName() {
        return myName;
    }

    public LispObject getValue() {
        return myValue;
    }

    public void setValue(LispObject myValue) {
        this.myValue = myValue;
    }

    public LispObject getFunction() {
        return myFunction;
    }

    public void setFunction(LispObject myFunction, String functionType) {
        setProperty("function-type", new LispString(functionType));
        this.myFunction = myFunction;
    }

    @Override
    public String toString() {
        return "LispSymbol{" +
                "myName='" + myName + '\'' +
                "myValue=[" + myValue.toString() + "] " +
                "myFunction=[" + myFunction.toString()+"]"+
                '}';
    }

    public boolean is (String name) {
        return myName.equals(name);
    }

    public boolean is (FunctionType functionType) {
        LispObject fType = getProperty("function-type");
        return fType.equals(new LispString(functionType.getValue()));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

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

    @Override
    public LispString toLispString() {
        return new LispString(myName);
    }

    @Override
    /**
     * takes Environment
     */
    public LispObject evaluate(Environment environment) {
        LispObject lispObject = environment.find(myName, "getValue");
        if (lispObject == null || lispObject.equals(LispSymbol.ourVoid))
            throw new VoidVariableException(myName);
        return lispObject;
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

    public LispString getVariableDocumentation () {
        return (LispString) getProperty("variable-documentation");
    }

    public void setVariableDocumentation (LispString value) {
        setProperty("variable-documentation", value);
    }


}
