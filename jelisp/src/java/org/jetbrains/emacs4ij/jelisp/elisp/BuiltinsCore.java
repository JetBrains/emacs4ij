package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsCore {
    private BuiltinsCore() {}

    @AnnotationBuiltin("+")
    public static LispObject plus (Environment environment, List<LispObject> args) {
        int ans = 0;
        for (LispObject lispObject: args) {
            if (lispObject.equals(LispSymbol.ourNil))
                break;
            ans += ((LispInteger)lispObject).getMyData();
        }
        return new LispInteger(ans);
    }
    @AnnotationBuiltin("*")
    public static LispObject multiply (Environment environment, List<LispObject> args) {
        int ans = 1;
        for (LispObject lispObject: args) {
            ans *= ((LispInteger)lispObject).getMyData();
        }
        return new LispInteger(ans);
    }
    @AnnotationBuiltin("set")
    public static LispObject set (Environment environment, List<LispObject> args) {
        if (args.size() != 2)
            throw new WrongNumberOfArgumentsException("set");
        LispSymbol variable;
        String varName = ((LispSymbol)args.get(0)).getName();
        try {
            variable = (LispSymbol) environment.find(varName);
            if (!variable.getValue().equals(args.get(1))) {
                variable.setValue(args.get(1));
                environment.defineSymbol(variable);
            }
        } catch (RuntimeException e) {
            variable = new LispSymbol(varName, args.get(1));
            environment.defineSymbol(variable);
        }
        return variable.getValue();
    }
    @AnnotationBuiltin("eq")
    public static LispObject eq (Environment environment, List<LispObject> args) {
        if (args.size() != 2)
            throw new WrongNumberOfArgumentsException("eq");
        if (args.get(0).equals(args.get(1)))
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }
    @AnnotationBuiltin("null")
    public static LispObject lispNull (Environment environment, List<LispObject> args) {
        if (args.size() != 1)
            throw new WrongNumberOfArgumentsException("null");
        return (args.get(0) == LispSymbol.ourNil) ? LispSymbol.ourT : LispSymbol.ourNil;
    }
    @AnnotationBuiltin("not")
    public static LispObject lispNot (Environment environment, List<LispObject> args) {
        //TODO another name
        return lispNull(environment, args);
    }
}



        //throw new RuntimeException("unknown builtin function " + function.getName());

