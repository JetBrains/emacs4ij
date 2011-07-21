package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Arrays;
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
    public static final LispSymbol ourNilSymbol = new LispSymbol("nil");
    public static final LispSymbol ourTSymbol = new LispSymbol("t");

    private String myName = null;

    public LispSymbol(String myName) {
        this.myName = myName;
    }

    public String getName() {
        return myName;
    }

    @Override
    public String toString() {
        return "LispSymbol{" +
                "myName='" + myName + '\'' +
                '}';
    }

    public boolean is (String name) {
        return myName.equals(name);
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
        return myName != null ? myName.hashCode() : 0;
    }

    @Override
    public LispString toLispString() {
        if (this.equals(ourNilSymbol))
            return new LispString("nil");
        if (this.equals(ourTSymbol))
            return new LispString("t");
        return new LispString(myName);
    }

    @Override
    public LispObject evaluate(Object... parameters) {
        Environment environment = null;
        try {
            environment =(Environment) Arrays.asList(parameters).get(0);
        } catch (ClassCastException e) {
            throw new RuntimeException("invalid symbol evaluation arguments!");
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
        LispObject lispObject = environment.find(myName, Environment.SymbolType.VARIABLE);
        if (lispObject == null)
            throw new VoidVariableException(myName);
        if ((lispObject == LispSymbol.ourNilSymbol) || (lispObject == LispSymbol.ourTSymbol))
            return lispObject;
        return lispObject.evaluate(parameters);
    }
}
