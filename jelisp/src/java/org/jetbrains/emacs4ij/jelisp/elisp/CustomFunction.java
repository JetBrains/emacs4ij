package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 11:07 AM
 * To change this template use File | Settings | File Templates.
 */
public abstract class CustomFunction {
    private LinkedHashMap<LispSymbol, String> myArguments = new LinkedHashMap<LispSymbol, String>();
    private LispString myDocString = null;
    private LispList myInteractive = null;
    private ArrayList<LispObject> myBody = new ArrayList<LispObject>();


    private void checkInteractiveAndBody (int index, List<LispObject> args) {
        try {
            if (((LispList)args.get(index)).car().equals(new LispSymbol("interactive"))) {
                myInteractive = (LispList)args.get(index);
                index++;
            }
        } catch (ClassCastException ignored) {
            //the body can be anything
        }
        for (int i = index; i != args.size(); ++i ) {
            myBody.add(args.get(i));
        }
    }

    private void setArguments (LispList args) {
        if (args.isEmpty())
            return;

        List<LispObject> data = args.getData();
        String type = "required";
        for (int i = 0, dataSize = data.size(); i < dataSize; i++) {
            LispSymbol argName = (LispSymbol)data.get(i);
            if (!type.equals("rest")) {
                if (argName.equals(new LispSymbol("&rest"))) {
                    type = "rest";
                    continue;
                }
                if (!type.equals("optional")) {
                    if (argName.equals(new LispSymbol("&optional"))) {
                        type = "optional";
                        continue;
                    }
                }
            }
            myArguments.put(argName, type);
        }
    }

    private void construct (List<LispObject> args) {
        //TODO: cast exceptions
        //myName = (LispSymbol)args.get(0);
        setArguments((LispList) args.get(1));
        if (args.size() == 2)
            return;
        if (args.get(2) instanceof LispString) {
            myDocString = (LispString)args.get(2);
            checkInteractiveAndBody(3, args);
        } else {
            checkInteractiveAndBody(2, args);
        }
    }

    public CustomFunction(LispObject... objects) {
        List<LispObject> args = Arrays.asList(objects);
        construct(args);
    }

    public CustomFunction(List<LispObject> args) {
        construct(args);
    }

    private void substituteArguments (Environment inner, List<LispObject> args) {
        List<LispSymbol> keys = new ArrayList<LispSymbol>(myArguments.keySet());
        if (!myArguments.isEmpty()) {
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LispObject argValue = args.get(i);
                LispSymbol argName =  keys.get(i);
                if (!myArguments.get(argName).equals("rest")) {
                    inner.defineSymbol(new LispSymbol(argName.getName(), argValue));
                    continue;
                }
                List<LispObject> rest = args.subList(i, argsSize);
                inner.defineSymbol(new LispSymbol(argName.getName(), new LispList(rest)));
                for (int k = i+1; k!=keys.size(); ++k)
                    inner.defineSymbol(new LispSymbol(keys.get(k).getName(), LispSymbol.ourNil));
                break;
            }
            for (int k = args.size(); k!=keys.size(); ++k)
                inner.defineSymbol(new LispSymbol(keys.get(k).getName(), LispSymbol.ourNil));
        }
    }

    private int nRequiredArguments() {
        int n = 0;
        for (Map.Entry<LispSymbol, String> arg: myArguments.entrySet()) {
            if (arg.getValue().equals("required"))
                ++n;
        }
        return n;
    }

    public static LispObject execute(LispSymbol function, Environment environment, List<LispObject> args) {
    /*    if (nRequiredArguments() > args.size() || myArguments.size() < args.size())
            throw new WrongNumberOfArgumentsException(myName.getName());
        Environment inner = new Environment(environment);
        substituteArguments(inner, args);
        LispObject result = LispSymbol.ourNil;
        for (int i=0; i!=myBody.size(); ++i) {
            result = myBody.get(i).evaluate(inner);
        }
        return result;  */
        return null;
    }

}
