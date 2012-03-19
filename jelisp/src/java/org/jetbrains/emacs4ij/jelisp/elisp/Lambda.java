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
public class Lambda implements FunctionCell, LispCommand {
    private List<LambdaArgument> myArgumentList = new LinkedList<>();
    private LispObject myDocumentation = null;
    private LispList myInteractive = null;
    private List<LispObject> myBody = new ArrayList<LispObject>();
    private int nRequiredArguments = 0;
    private boolean infiniteArgs = false;
    private int nKeywords = 0;

    public Lambda (LispList def) {
        List<LispObject> data = def.toLispObjectList();
        if (!data.get(0).equals(new LispSymbol("lambda")))
            throw new RuntimeException("wrong lambda definition");
        try {
            if (!data.get(1).equals(LispSymbol.ourNil))
                parseArgumentsList((LispList) data.get(1));
        } catch (ClassCastException e) {
            throw new InvalidFunctionException(def.toString());
        }
        if (data.size() > 2) {
//            int index = 2;
            try {
                //todo: if those instructions have some side effects, this is wrong behaviour
                //LispObject docString = data.get(2).evaluate(environment);
                LispObject docString = data.get(2);
                if (docString instanceof LispString) {
                    myDocumentation = docString;
//                    index = 3;
                }
            } catch (LispException e) {
                myDocumentation = null;
            }


            myBody = data.subList(2, data.size());

            for (LispObject bodyForm: myBody) {
                if (bodyForm instanceof LispList && !((LispList)bodyForm).isEmpty()) {
                    if (((LispList)bodyForm).car().equals(new LispSymbol("interactive")) && myInteractive == null) {
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
        List<LispObject> data = args.toLispObjectList();
        LambdaArgument.Type type = LambdaArgument.Type.REQUIRED;
        for (LispObject aData : data) {
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
        }
    }

    @Override
    public String toString() {
        String s = "(lambda " + (myArgumentList.isEmpty() ? "nil" : myArgumentList.toString());
        for (LispObject bodyForm: myBody) {
            s += " " + bodyForm.toString();
        }
        s += ")";
        return s;
    }

    @Override
    public LispObject evaluate(Environment environment) {
        throw new RuntimeException("wrong usage");
    }

    private LispObject evaluateBody (CustomEnvironment inner) {
        LispObject result = LispSymbol.ourNil;
        for (LispObject bodyForm: myBody) {
            result = bodyForm.evaluate(inner);
        }
        return result;
    }

    public LispObject evaluate(Environment environment, List<LispObject> args) {
        return evaluateBody(substituteArguments(environment, args));
    }
    
    private boolean checkOversize(int n) {
        return !infiniteArgs && myArgumentList.size() + nKeywords * 2 < n;
    }

    public CustomEnvironment substituteArguments (Environment environment, List<LispObject> args) {
        if (nRequiredArguments > args.size() || checkOversize(args.size()))
            throw new WrongNumberOfArgumentsException(toString(), args.size());

        CustomEnvironment inner = new CustomEnvironment(environment);
        if (!myArgumentList.isEmpty()) {
            int j = args.size();
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LispObject argValue = args.get(i);
                LambdaArgument argument = myArgumentList.get(i);
                if (argument.getType() == LambdaArgument.Type.REQUIRED || argument.getType() == LambdaArgument.Type.OPTIONAL) {
                    argument.setValue(inner, argValue);
                    continue;
                }
                if (argument.getType() == LambdaArgument.Type.KEYWORD) {                    
                    LispObject keyword = argValue.evaluate(inner); //todo: if this code has side effects, we shouldn't do this!
                    if (!(keyword instanceof LispSymbol))
                        throw new WrongTypeArgumentException("keyword", keyword.getClass().getSimpleName()); 
                    if (!argument.getKeyword().equals(keyword)) {
                        //can the keywords go in random order? Now can't. + this saves us from unknown keywords
                        throw new WrongTypeArgumentException(argument.getKeyword().getName(), ((LispSymbol) keyword).getName());
                    }
                    if (i+1 >= argsSize) {
                        throw new InternalError("Keyword with no value: " + keyword);
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
//        System.out.println("--------");
        return inner;
    }

    @Override
    public LispObject getDocumentation() {
        return myDocumentation == null ? LispSymbol.ourNil : myDocumentation;
    }

    @Override
    public void setDocumentation(LispObject doc) {
        myDocumentation = doc;
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
        if (args.toLispObjectList().get(0) instanceof LispString) {
            return ((LispString) args.toLispObjectList().get(0)).getData();
        }
        return null;
    }

    @Override
    public int getNRequiredArguments() {
        return nRequiredArguments;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Lambda)) return false;

        Lambda lambda = (Lambda) o;

        if (infiniteArgs != lambda.infiniteArgs) return false;
        if (nKeywords != lambda.nKeywords) return false;
        if (nRequiredArguments != lambda.nRequiredArguments) return false;
        if (myArgumentList != null ? !myArgumentList.equals(lambda.myArgumentList) : lambda.myArgumentList != null)
            return false;
        if (myBody != null ? !myBody.equals(lambda.myBody) : lambda.myBody != null) return false;
        if (myDocumentation != null ? !myDocumentation.equals(lambda.myDocumentation) : lambda.myDocumentation != null)
            return false;
        if (myInteractive != null ? !myInteractive.equals(lambda.myInteractive) : lambda.myInteractive != null)
            return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myArgumentList != null ? myArgumentList.hashCode() : 0;
        result = 31 * result + (myDocumentation != null ? myDocumentation.hashCode() : 0);
        result = 31 * result + (myInteractive != null ? myInteractive.hashCode() : 0);
        result = 31 * result + (myBody != null ? myBody.hashCode() : 0);
        result = 31 * result + nRequiredArguments;
        result = 31 * result + (infiniteArgs ? 1 : 0);
        result = 31 * result + nKeywords;
        return result;
    }

    @Override
    public LispList getInteractiveForm() {
        return myInteractive == null ? LispList.list() : myInteractive;
    }
}
