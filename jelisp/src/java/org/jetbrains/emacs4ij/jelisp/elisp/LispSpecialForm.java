package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.HashMap;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 *
 * in fact it is a kind of builtin function
 */
public class LispSpecialForm extends LispFunction {

    public LispSpecialForm(String myName) {
        super(myName);
    }

    private void bindLetVariables (boolean isStar, Environment inner, LispList varList) {
        HashMap<LispSymbol, LispObject> vars = new HashMap<LispSymbol, LispObject>();
        for (LispObject var: varList.getData()) {
            if (var instanceof LispList) {
                LispSymbol symbol = (LispSymbol) ((LispList) var).car();
                LispList valueForm = (LispList) ((LispList) var).cdr();
                LispObject value = valueForm.car().evaluate(inner);

                if (isStar)
                    inner.setVariable(symbol, value);
                else
                    vars.put(symbol, value);

                continue;
            }
            if (var instanceof LispSymbol) {
                if (isStar)
                    inner.setVariable(var, LispSymbol.ourNil);
                else
                    vars.put((LispSymbol)var, LispSymbol.ourNil);

                continue;
            }
            throw new RuntimeException("wrong variable " + var.toString());
        }

        if (!isStar)
            for (LispSymbol symbol : vars.keySet()) {
                inner.setVariable(symbol, vars.get(symbol));
            }
    }

    private LispObject executeLet (boolean isStar, Environment environment, List<LispObject> args) {
        /* (let(*) VARLIST BODY...)
        Bind variables according to VARLIST then eval BODY.
        The value of the last form in BODY is returned.
        Each element of VARLIST is a symbol (which is bound to nil)
        or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).

        let:  All the VALUEFORMs are evalled before any symbols are bound.
        let*: Each VALUEFORM can refer to the symbols already bound by this VARLIST.
        */
        Environment inner = new Environment(environment);
        LispList varList = (LispList) args.get(0);
        bindLetVariables(isStar, inner, varList);

        // eval body
        LispObject result = LispSymbol.ourNil;
        for (int i=1; i!=args.size(); ++i) {
            result = args.get(i).evaluate(inner);
        }
        return result;
    }

    @Override
    public LispObject execute (Environment environment, List<LispObject> args) throws WrongNumberOfArgumentsException {
        if (myName.is("quote")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            return args.get(0);
        }
        if (myName.is("defun")) {
            if (args.size() < 2)
                throw new WrongNumberOfArgumentsException(myName.getName());
            LispCustomFunction function = new LispCustomFunction(args);
            environment.defineFunction(function.getName(), function);
            return function.getName();
        }
        if (myName.is("let")) {
            return executeLet(false, environment, args);
        }
        if (myName.is("let*")) {
            return executeLet(true, environment, args);
        }
        if (myName.is("or")) {
            for (int i=0; i!=args.size(); ++i) {
                LispObject result = args.get(i).evaluate(environment);
                if (result != LispSymbol.ourNil)
                    return result;
            }
            return LispSymbol.ourNil;
        }
        if (myName.is("and")) {
            LispObject result = LispSymbol.ourT;
            for (int i=0; i!=args.size(); ++i) {
                result = args.get(i).evaluate(environment);
                if (result == LispSymbol.ourNil)
                    return result;
            }
            return result;
        }
        if (myName.is("if")) {
            if (args.size() < 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            LispObject condition = args.get(0).evaluate(environment);
            if (condition != LispSymbol.ourNil) {
                if (args.size() > 1)
                    return args.get(1).evaluate(environment);
                return LispSymbol.ourT;
            }
            LispObject result = LispSymbol.ourNil;
            for (int i=2; i<args.size(); ++i) {
                result = args.get(i).evaluate(environment);
            }
            return result;
        }
        if (myName.is("interactive")) {
            throw new NotImplementedException();
        }

        throw new RuntimeException("unknown special form " + myName);
    }

    @Override
    public LispString toLispString() {
        return null;
    }
}
