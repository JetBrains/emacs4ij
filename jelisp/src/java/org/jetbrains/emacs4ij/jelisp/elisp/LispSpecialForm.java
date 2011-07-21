package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Kate
 * Date: 7/16/11
 * Time: 2:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class LispSpecialForm extends LispFunction {

    public LispSpecialForm(String myName) {
        super(myName);
    }

    public LispObject execute (List<LispObject> args, Environment environment) throws WrongNumberOfArgumentsException {
        if (myName.is("quote")) {
            if (args.size() != 1)
                throw new WrongNumberOfArgumentsException();
            return args.get(0);
        }
        if (myName.is("defun")) {
            if (args.size() < 3)
                throw new WrongNumberOfArgumentsException();
            LispCustomFunction function = new LispCustomFunction(args);
            environment.defineFunction(function.getName(), function);
            return function.getName();
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
