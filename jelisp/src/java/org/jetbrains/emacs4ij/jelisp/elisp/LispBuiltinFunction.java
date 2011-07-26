package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Arrays;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/13/11
 * Time: 3:49 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispBuiltinFunction extends LispFunction {

    public LispBuiltinFunction(String myName) {
        super(myName);
    }

    public LispObject execute (Environment environment, List<LispObject> args) {
        if (myName.is("+")) {
            int ans = 0;
            for (LispObject lispObject: args) {
                ans += ((LispInteger)lispObject).getMyData();
            }
            return new LispInteger(ans);
        }
        if (myName.is("*")) {
            int ans = 1;
            for (LispObject lispObject: args) {
                ans *= ((LispInteger)lispObject).getMyData();
            }
            return new LispInteger(ans);
        }
        if (myName.is("set")) {
            if (args.size() != 2)
                throw new WrongNumberOfArgumentsException(myName.getName());
            environment.setVariable(args.get(0), args.get(1));
            return args.get(1);
        }
        if (myName.is("eq")) {
            if (args.size() != 2)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (args.get(0).equals(args.get(1)))
                return LispSymbol.ourT;
            return LispSymbol.ourNil;
        }
        if (myName.is("car-safe")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (args.get(0) instanceof LispList)
                return ((LispList) args.get(0)).car();
            return LispSymbol.ourNil;
        }
        if (myName.is("cdr-safe")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (args.get(0) instanceof LispList)
                return ((LispList) args.get(0)).cdr();
            return LispSymbol.ourNil;
        }
        if (myName.is("memq")) {
            if (args.size() != 2)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (args.get(1) instanceof LispList) {
                return ((LispList) args.get(1)).memq(args.get(0));
            }
            throw new WrongTypeArgument("LispList", args.get(1).getClass().toString());
        }
        if (myName.is("list")) {
            LispList list = new LispList(args);
            return list.isEmpty() ? LispSymbol.ourNil : list;
        }



        throw new RuntimeException("unknown builtin function " + myName);
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName());
    }

}
