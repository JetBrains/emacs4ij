package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/1/11
 * Time: 1:01 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispVariable extends LispObject {
    private LispObject myValue = LispSymbol.ourVoid;
    protected LispSymbol myName = null;

    public LispSymbol getName() {
        return myName;
    }

    public LispVariable (LispSymbol name, LispObject value) {
        myName = name;
        myValue = value;
    }

    public LispVariable (Environment environment, List<LispObject> args) {
        myName = (LispSymbol) args.get(0);
        myValue = (args.size() > 1) ? args.get(1).evaluate(environment) : LispSymbol.ourVoid;
        if (args.size() == 3) {
            LispString docString = (LispString) args.get(2);
            myName.setProperty(new LispSymbol("variable-documentation"), docString);
        }
    }

    public static void createOrUpdate (Environment environment, List<LispObject> args) {
        String name = ((LispSymbol) args.get(0)).getName();
        LispSymbol variable;
        try {
            variable = (LispSymbol) environment.find(name);
            if (variable.getValue().equals(LispSymbol.ourVoid) && (args.size() > 1))
                variable.setValue(args.get(1).evaluate(environment));
            if (args.size() == 3) {
                LispString docString = (LispString) args.get(2);
                if (!(variable.getVariableDocumentation().equals(docString)))
                    variable.setVariableDocumentation(docString);
            }
        } catch (VoidVariableException e) {
            //variable = new LispVariable(environment, args);
            variable = null;
        }
        environment.defineSymbol(variable);
    }

    public LispObject getValue() {
        return myValue;
    }

    public LispObject getDocString() {
        return myName.getProperty(new LispSymbol("variable-documentation"));
    }

    public void setDocString(LispString docString) {
        myName.setProperty(new LispSymbol("variable-documentation"), docString);
    }

    public void setValue(LispObject myValue) {
        this.myValue = myValue;
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName() + " = " + myValue.toString());
    }

    @Override
    public LispObject evaluate(Environment environment) {
        if (myValue.equals(LispSymbol.ourVoid))
            throw new VoidVariableException(myName.getName());
        return myValue;
    }

    @Override
    public String toString() {
        return "LispVariable{" +
                "myName=" + myName +
                ", myValue=" + myValue +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispVariable that = (LispVariable) o;

        if (myName != null ? !myName.equals(that.myName) : that.myName != null) return false;
        if (myValue != null ? !myValue.equals(that.myValue) : that.myValue != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myName != null ? myName.hashCode() : 0;
        result = 31 * result + (myValue != null ? myValue.hashCode() : 0);
        return result;
    }
}
