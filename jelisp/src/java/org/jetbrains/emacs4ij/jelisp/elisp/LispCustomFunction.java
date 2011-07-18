package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.Evaluator;

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
    private LispObject myArguments = null;
    private LispString myDocumentation = null;
    private LispList myInteractive = null;
    private LispList myBody = null;

    public LispCustomFunction(List<LispObject> args) {
        //TODO: cast exceptions
        //TODO: empty body?
        myName = (LispSymbol)args.get(0);
        myArguments = args.get(1);
        if (args.get(2) instanceof LispString) {
            myDocumentation = (LispString)args.get(2);
            if (args.size() == 4) {
                myBody = (LispList)args.get(3);
            } else {
                myInteractive = (LispList)args.get(3);
                myBody = (LispList)args.get(4);
            }
        } else {
            if (args.size() == 3) {
                myBody = (LispList)args.get(2);
            } else {
                myInteractive = (LispList)args.get(2);
                myBody = (LispList)args.get(3);
            }
        }
    }


    @Override
    public LispObject execute(List<LispObject> args, Environment environment) {
        //TODO: check for number of arguments

        if (myArguments != LispSymbol.ourNilSymbol) {
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LispObject argValue = args.get(i);
                LispObject argMask = ((LispList)myArguments).get(i);
                Collections.replaceAll(myBody.getData(), argMask, argValue);
            }
        }

        return Evaluator.evaluate(myBody, new Environment(environment));
    }

    public LispSymbol getName() {
        return myName;
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName());
    }
}
