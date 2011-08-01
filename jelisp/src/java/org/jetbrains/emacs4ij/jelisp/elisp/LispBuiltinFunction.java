package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgument;

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
                if (lispObject.equals(LispSymbol.ourNil))
                    break;
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
            LispVariable variable;
            try {
                variable = (LispVariable) environment.find(((LispSymbol)args.get(0)).getName(), Environment.SymbolType.VARIABLE);
                if (!variable.getValue().equals(args.get(1))) {
                    variable.setValue(args.get(1));
                    environment.defineVariable(args.get(0), variable);
                }
            } catch (VoidVariableException e) {
                variable = new LispVariable((LispSymbol) args.get(0), args.get(1));
                environment.defineVariable(args.get(0), variable);
            }
            return variable.getValue();
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
        if (myName.is("null") || myName.is("not")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            return (args.get(0) == LispSymbol.ourNil) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (myName.is("car")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (!(args.get(0) instanceof LispList))
                throw new WrongTypeArgument("LispList", args.get(0).getClass().toString());
            return ((LispList) args.get(0)).car();
        }
        if (myName.is("cdr")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (!(args.get(0) instanceof LispList))
                throw new WrongTypeArgument("LispList", args.get(0).getClass().toString());
            LispList cdr = ((LispList) args.get(0)).cdr();
            return (cdr.isEmpty()) ? LispSymbol.ourNil : cdr;
        }
        if (myName.is("stringp")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            return (args.get(0) instanceof LispString) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (myName.is("symbolp")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            return (args.get(0) instanceof LispSymbol) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (myName.is("integerp")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            return (args.get(0) instanceof LispInteger) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (myName.is("symbol-function")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if (!(args.get(0) instanceof LispSymbol))
                throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString());
            try {
                return environment.find(((LispSymbol) args.get(0)).getName(), Environment.SymbolType.FUNCTION, "getDefinition");
            } catch (RuntimeException e) {
                throw new VoidFunctionException(((LispSymbol)args.get(0)).getName());
            }
        }
        if (myName.is("subrp")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException(myName.getName());
            return (args.get(0) instanceof LispFunction) ? LispSymbol.ourT : LispSymbol.ourNil;
        }
        if (myName.is("get")) {
            if (args.size() != 2)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if ((!(args.get(0) instanceof LispSymbol)) || (!(args.get(1) instanceof LispSymbol)))
                throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString() + " and " + args.get(1).getClass().toString());
            LispSymbol symbol = (LispSymbol) args.get(0);
            LispSymbol property = (LispSymbol) args.get(1);
            return environment.find(symbol.getName(), Environment.SymbolType.ANY, "getProperty", new Class[]{LispSymbol.class}, property);
        }
        if (myName.is("put")) {
            if (args.size() != 3)
                throw new WrongNumberOfArgumentsException(myName.getName());
            if ((!(args.get(0) instanceof LispSymbol)) || (!(args.get(1) instanceof LispSymbol)))
                throw new WrongTypeArgument("LispSymbol", args.get(0).getClass().toString() + " and " + args.get(1).getClass().toString());
            LispSymbol symbol = (LispSymbol) args.get(0);
            LispSymbol property = (LispSymbol) args.get(1);
            LispObject value = args.get(2);

            environment.find(symbol.getName(), Environment.SymbolType.ANY, "setProperty", new Class[] {LispSymbol.class, LispObject.class}, property, value);
            return value;
        }


        throw new RuntimeException("unknown builtin function " + myName.getName());
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName());
    }

}
