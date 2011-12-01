package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/6/11
 * Time: 5:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class Lambda extends LispObject {
    private LinkedHashMap<LispSymbol, String> myArgumentList = null;
    private LispString myDocString = null;
    private LispList myInteractive = null;
    private List<LObject> myBody = new ArrayList<LObject>();

    public Lambda (LispList def, Environment environment) {
        List<LObject> data = def.getData();
        if (!data.get(0).equals(new LispSymbol("lambda")))
            throw new RuntimeException("wrong lambda definition");
        try {
            parseArgumentsList((LispList) data.get(1));
        } catch (ClassCastException e) {
            throw new InvalidFunctionException(def.toString());
        }
        if (data.size() > 2) {
            try {
                //todo: if those instructions have some side effects, this is wrong behaviour
                //LObject docString = data.get(2).evaluate(environment);
                LObject docString = data.get(2);
                if (docString instanceof LispString) {
                    myDocString = (LispString) docString;
                }
            } catch (LispException e) {
                myDocString = null;
            }
            myBody = data.subList(2, data.size());

            for (LObject bodyForm: myBody) {
                if (bodyForm instanceof LispList) {
                    if (((LispList)bodyForm).car().equals(new LispSymbol("interactive"))) {
                        myInteractive = (LispList) bodyForm;
                    }
                }
            }
        }
    }

    public void parseArgumentsList (LispList args) {
        myArgumentList = new LinkedHashMap<LispSymbol, String>();
        if (args.isEmpty())
            return;

        List<LObject> data = args.getData();
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
            myArgumentList.put(argName, type);
        }
    }

    @Override
    public String toString() {
        String s = "(lambda " + myArgumentList.toString();
        for (LObject bodyForm: myBody) {
            s += " " + bodyForm.toString();
        }
        s += ")";
        return s;
    }

    @Override
    public LObject evaluate(Environment environment) {
        throw new RuntimeException("wrong usage");
    }

    private LObject evaluateBody (Environment inner) {
        LObject result = LispSymbol.ourNil;
        for (LObject bodyForm: myBody) {
            result = bodyForm.evaluate(inner);
        }
        return result;
    }

    public LObject evaluate(Environment environment, List<LObject> args) {
        return evaluateBody(substituteArguments(environment, args));
    }

    public Environment substituteArguments (Environment environment, List<LObject> args) {
        int nRequiredArguments = 0;
        for (Map.Entry<LispSymbol, String> arg: myArgumentList.entrySet()) {
            if (arg.getValue().equals("required"))
                ++nRequiredArguments;
        }
        if (nRequiredArguments > args.size() || myArgumentList.size() < args.size())
            throw new WrongNumberOfArgumentsException(toString(), args.size());

        Environment inner = new Environment(environment);
        List<LispSymbol> keys = new ArrayList<LispSymbol>(myArgumentList.keySet());
        if (!myArgumentList.isEmpty()) {
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LObject argValue = args.get(i);
                LispSymbol argName =  keys.get(i);
                if (!myArgumentList.get(argName).equals("rest")) {
                    inner.defineSymbol(new LispSymbol(argName.getName(), argValue));
                    continue;
                }
                List<LObject> rest = args.subList(i, argsSize);
                inner.defineSymbol(new LispSymbol(argName.getName(), new LispList(rest)));
                for (int k = i+1; k!=keys.size(); ++k)
                    inner.defineSymbol(new LispSymbol(keys.get(k).getName(), LispSymbol.ourNil));
                break;
            }
            for (int k = args.size(); k!=keys.size(); ++k)
                inner.defineSymbol(new LispSymbol(keys.get(k).getName(), LispSymbol.ourNil));
        }
        return inner;
    }

    public LispObject getDocString () {
        return myDocString == null ? LispSymbol.ourNil : myDocString;
    }

    public boolean isInteractive() {
        return myInteractive != null;
    }

    public String getInteractiveString () {
        LispList args = myInteractive.cdr();
        if (args.isEmpty())
            return null;
        if (args.getData().get(0) instanceof LispString) {
            return ((LispString) args.getData().get(0)).getData();
        }
        return null;
    }
}
