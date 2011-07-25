package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Evaluator;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 11:07 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispCustomFunction extends LispFunction {
    private LispList myArguments = null;
    private LispString myDocumentation = null;
    private LispList myInteractive = null;
    private ArrayList<LispObject> myBody = new ArrayList<LispObject>();

    private void checkInteractiveAndBody (int index, List<LispObject> args) {
        try {
            if (((LispList)args.get(index)).car() == new LispSymbol("interactive")) {
                myInteractive = (LispList)args.get(index);
                index++;
            }
        } catch (ClassCastException ignored) {
            //args at index can be any LispObject
        }
        for (int i = index; i != args.size(); ++i ) {
            myBody.add(args.get(i));
        }
    }

    public LispCustomFunction(List<LispObject> args) {
        //TODO: cast exceptions
        myName = (LispSymbol)args.get(0);
        myArguments = (LispList)args.get(1);
        if (args.get(2) instanceof LispString) {
            myDocumentation = (LispString)args.get(2);
            checkInteractiveAndBody(3, args);
        } else {
            checkInteractiveAndBody(2, args);
        }
    }

    private void substituteArgument (LispObject argName, LispObject argValue) {
        for (LispObject bodyElement: myBody) {
            if (bodyElement == argName) {
                //TODO: check
                bodyElement = argValue;
                continue;
            }
            if (bodyElement instanceof LispList) {
                Collections.replaceAll(((LispList) bodyElement).getData(), argName , argValue);
                continue;
            }
            throw new RuntimeException("argument substitution failed: function " + myName + ", argument " + argName);
        }
    }

    @Override
    public LispObject execute(List<LispObject> args, Environment environment) {

        if (myArguments.getSize() != args.size())
            throw new WrongNumberOfArgumentsException();

        if (!myArguments.isEmpty()) {
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LispObject argValue = args.get(i);
                LispObject argName = myArguments.get(i);
                substituteArgument(argName , argValue);
            }
        }

        LispObject result = LispSymbol.ourNil;
        for (int i=0; i!=myBody.size(); ++i) {
            result = Evaluator.evaluate(myBody.get(i), new Environment(environment));
        }
        return result;
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName());
    }

}
