package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.CustomEnvironment;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.LispException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongTypeArgumentException;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: kate
 * Date: 10/6/11
 * Time: 5:46 PM
 * To change this template use File | Settings | File Templates.
 */
public class Lambda extends LispObject implements FunctionCell {
    private LinkedList<LambdaArgument> myArgumentList = null;
    private LispString myDocString = null;
    private LispList myInteractive = null;
    private List<LObject> myBody = new ArrayList<LObject>();
    private int nRequiredArguments;
    private boolean infiniteArgs = false;
    private int nKeywords = 0;
    private int nOptional = 0;

    public Lambda (LispList def, Environment environment) {
        List<LObject> data = def.toLObjectList();
        if (!data.get(0).equals(new LispSymbol("lambda")))
            throw new RuntimeException("wrong lambda definition");
        try {
            parseArgumentsList((LispList) data.get(1));
        } catch (ClassCastException e) {
            throw new InvalidFunctionException(def.toString());
        }
        if (data.size() > 2) {
         //   int index = 2;
            try {
                //todo: if those instructions have some side effects, this is wrong behaviour
                //LObject docString = data.get(2).evaluate(environment);
                LObject docString = data.get(2);
                if (docString instanceof LispString) {
                    myDocString = (LispString) docString;
                    //index = 3;
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

    //todo: for test only
    public List<LambdaArgument> getArguments () {
        return myArgumentList;
    }
    
    public void parseArgumentsList (LispList args) {
        nRequiredArguments = 0;
        myArgumentList = new LinkedList<>();
        if (args.isEmpty())
            return;
        List<LObject> data = args.toLObjectList();
        LambdaArgument.Type type = LambdaArgument.Type.REQUIRED;
        for (LObject aData : data) {
            if (aData instanceof LispSymbol) {
                if (aData.equals(new LispSymbol("&rest"))) {
                    type = LambdaArgument.Type.REST;
                    infiniteArgs = true;
                    continue;
                }
                if (aData.equals(new LispSymbol("&optional"))) {
                    type = LambdaArgument.Type.OPTIONAL;
                    continue;
                }
                if (aData.equals(new LispSymbol("&key"))) {
                    type = LambdaArgument.Type.KEYWORD;
                    continue;
                }
            }
            myArgumentList.add(new LambdaArgument(type, aData, "lambda"));
            if (type == LambdaArgument.Type.REQUIRED)
                nRequiredArguments++;
            else if (type == LambdaArgument.Type.KEYWORD)
                nKeywords++;
            else if (type == LambdaArgument.Type.OPTIONAL)
                nOptional++;
        }
    }

    @Override
    public String toString() {
        String s = "(lambda " + (myArgumentList.isEmpty() ? "nil" : myArgumentList.toString());
        boolean first = true;
        for (LObject bodyForm: myBody) {
            if (first && bodyForm instanceof LispString) { //todo: remove this clause == don't print doc for debug
                first = false;
                continue;
            }
            s += " " + bodyForm.toString();
        }
        s += ")";
        return s;
    }

    @Override
    public LObject evaluate(Environment environment) {
        throw new RuntimeException("wrong usage");
    }

    private LObject evaluateBody (CustomEnvironment inner) {
        LObject result = LispSymbol.ourNil;
        for (LObject bodyForm: myBody) {
            result = bodyForm.evaluate(inner);
        }
        return result;
    }

    public LObject evaluate(Environment environment, List<LObject> args) {
        return evaluateBody(substituteArguments(environment, args));
    }
    
    private boolean checkOversize(int n) {
        return !infiniteArgs && myArgumentList.size() + nKeywords * 2 < n;
    }

    public CustomEnvironment substituteArguments (Environment environment, List<LObject> args) {
        if (nRequiredArguments > args.size() || checkOversize(args.size()))
            throw new WrongNumberOfArgumentsException(toString(), args.size());

        CustomEnvironment inner = new CustomEnvironment(environment);
        if (!myArgumentList.isEmpty()) {
            int j = args.size();
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LObject argValue = args.get(i);
                LambdaArgument argument = myArgumentList.get(i);
                if (argument.getType() == LambdaArgument.Type.REQUIRED || argument.getType() == LambdaArgument.Type.OPTIONAL) {
                    argument.setValue(inner, argValue);
                    continue;
                }
                if (argument.getType() == LambdaArgument.Type.KEYWORD) {                    
                    LObject keyword = argValue.evaluate(inner); //todo: if this code has side effects, we shouldn't do this!
                    if (!(keyword instanceof LispSymbol))
                        throw new WrongTypeArgumentException("keyword", keyword.getClass().getSimpleName()); 
                    if (!argument.getKeyword().equals(keyword)) {
                        //can the keywords go in random order? Now can't. + this saves us from unknown keywords
                        throw new WrongTypeArgumentException(argument.getKeyword().getName(), ((LispSymbol) keyword).getName());
                    }
                    if (i+1 >= argsSize) {
                        throw new RuntimeException("Keyword with no value: " + keyword);
                    }
                    argument.setValue(inner, args.get(i+1));
                    i++;
                    continue;
                }
                argument.setValue(inner, LispList.list(args.subList(i, argsSize)));
                j = i + 1;
                break;
            }
            for (int k = j; k < myArgumentList.size(); ++k)
                myArgumentList.get(k).setValue(inner, null);
        }
        return inner;
    }

    @Override
    public LispObject getDocString () {
        return myDocString == null ? LispSymbol.ourNil : myDocString;
    }

    @Override
    public boolean isInteractive() {
        return myInteractive != null;
    }

    @Override
    public String getInteractiveString () {
        LispList args = (LispList) myInteractive.cdr();
        if (args.isEmpty())
            return null;
        if (args.toLObjectList().get(0) instanceof LispString) {
            return ((LispString) args.toLObjectList().get(0)).getData();
        }
        return null;
    }
}
