package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 2/2/12
 * Time: 1:50 PM
 * To change this template use File | Settings | File Templates.
 */
public class LambdaArgument {
    private LispSymbol myKeyword = LispSymbol.ourNil;
    private LispSymbol myVar;
    private LObject myInitForm = LispSymbol.ourNil;
    private LispSymbol mySetVar = LispSymbol.ourNil;
    public enum Type {REQUIRED, OPTIONAL, REST, KEYWORD}
    private Type myType;

    public LambdaArgument (Type type, LObject arg, String fName) {
        myType = type;
        if (arg instanceof LispList) {
            List<LObject> list = ((LispList) arg).toLObjectList();
            if (myType == Type.KEYWORD && list.get(0) instanceof LispList) {
                List<LObject> def = ((LispList) list.get(0)).toLObjectList();
                if (def.size() != 2 || !(def.get(0) instanceof LispSymbol) || !(def.get(1) instanceof LispSymbol))
                    throw new InvalidFunctionException(fName);
                myKeyword = (LispSymbol) def.get(0);
                myVar = (LispSymbol) def.get(1);
            } else if (list.get(0) instanceof LispSymbol) {
                myKeyword = new LispSymbol(':'+ ((LispSymbol) list.get(0)).getName());
                myVar = (LispSymbol) list.get(0);
            } else {
                throw new InvalidFunctionException(fName);
            }
            if (list.size() > 1) {
                myInitForm = list.get(1);
                if (list.size() == 3) {
                    if (list.get(2) instanceof LispSymbol)
                        mySetVar = (LispSymbol) list.get(2);
                    else throw new InvalidFunctionException(fName);
                } else if (list.size() > 3) throw new InvalidFunctionException(fName);
            }
        } else if (arg instanceof LispSymbol) {
            myVar = (LispSymbol) arg;
        } else
            throw new InvalidFunctionException(fName);
    }

    public Type getType() {
        return myType;
    }

    public LispSymbol getKeyword() {
        if (myType == Type.KEYWORD)
            return myKeyword;
        return null;
    }

    //todo: for test only
    public LispSymbol getVar() {
        return myVar;
    }

    //todo: for test only
    public LispSymbol getSetVar() {
        return mySetVar;
    }

    //todo: for test only
    public LObject getInitForm() {
        return myInitForm;
    }

    public void setValue (Environment inner, @Nullable LObject value) {
        myVar = new LispSymbol(myVar.getName());
        if (value == null) {
            myVar.setValue(myInitForm.evaluate(inner));
            if (!mySetVar.equals(LispSymbol.ourNil))
                mySetVar = new LispSymbol(mySetVar.getName(), LispSymbol.ourNil);
            inner.defineSymbol(myVar);
            //return;
        } else {
            myVar.setValue(value);
            if (!mySetVar.equals(LispSymbol.ourNil))
                mySetVar = new LispSymbol(mySetVar.getName(), LispSymbol.ourT);
            inner.defineSymbol(myVar);
        }
        System.out.println("    " + myVar.toString());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LambdaArgument)) return false;

        LambdaArgument LambdaArgument = (LambdaArgument) o;

        if (myInitForm != null ? !myInitForm.equals(LambdaArgument.myInitForm) : LambdaArgument.myInitForm != null)
            return false;
        if (myKeyword != null ? !myKeyword.equals(LambdaArgument.myKeyword) : LambdaArgument.myKeyword != null) return false;
        if (mySetVar != null ? !mySetVar.equals(LambdaArgument.mySetVar) : LambdaArgument.mySetVar != null) return false;
        if (myType != LambdaArgument.myType) return false;
        if (myVar != null ? !myVar.equals(LambdaArgument.myVar) : LambdaArgument.myVar != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myKeyword != null ? myKeyword.hashCode() : 0;
        result = 31 * result + (myVar != null ? myVar.hashCode() : 0);
        result = 31 * result + (myInitForm != null ? myInitForm.hashCode() : 0);
        result = 31 * result + (mySetVar != null ? mySetVar.hashCode() : 0);
        result = 31 * result + (myType != null ? myType.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "LambdaArgument{" +
                "myKeyword=" + myKeyword +
                ", myVar=" + myVar +
                ", myInitForm=" + myInitForm +
                ", mySetVar=" + mySetVar +
                ", myType=" + myType +
                '}';
    }
}
