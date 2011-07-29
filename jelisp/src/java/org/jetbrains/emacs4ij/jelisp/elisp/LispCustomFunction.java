package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/18/11
 * Time: 11:07 AM
 * To change this template use File | Settings | File Templates.
 */
public class LispCustomFunction extends LispFunction {
    //private LinkedHashMap<LispSymbol, LispObject> myParameters = new LinkedHashMap<LispSymbol, LispObject>();
    private LinkedHashMap<LispSymbol, String> myArguments = new LinkedHashMap<LispSymbol, String>();
    private LispString myDocumentation = null;
    private LispList myInteractive = null;
    private ArrayList<LispObject> myBody = new ArrayList<LispObject>();

    private void checkInteractiveAndBody (int index, List<LispObject> args) {
        try {
            if (((LispList)args.get(index)).car().equals(new LispSymbol("interactive"))) {
                myInteractive = (LispList)args.get(index);
                index++;
            }
        } catch (ClassCastException ignored) {

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
        myName = (LispSymbol)args.get(0);
        setArguments((LispList) args.get(1));
        if (args.size() == 2)
            return;
        if (args.get(2) instanceof LispString) {
            myDocumentation = (LispString)args.get(2);
            checkInteractiveAndBody(3, args);
        } else {
            checkInteractiveAndBody(2, args);
        }
    }

    public LispCustomFunction (LispObject ... objects) {
        List<LispObject> args = Arrays.asList(objects);
        construct(args);
    }

    public LispCustomFunction(List<LispObject> args) {
        construct(args);
    }

    private void substituteArgument (ArrayList<LispObject> where, LispObject argName, LispObject argValue) {
        for (int i = 0, myBodySize = where.size(); i < myBodySize; i++) {
            if (where.get(i).equals(argName)) {
                where.set(i, argValue);
                continue;
            }
            if (where.get(i) instanceof LispList) {
                substituteArgument((ArrayList<LispObject>) ((LispList) where.get(i)).getData(), argName, argValue);
                //continue;
            }
//            throw new RuntimeException("argument substitution failed: function " + myName + ", argument " + argName);
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

    @Override
    public LispObject execute(Environment environment, List<LispObject> args) {

        if (nRequiredArguments() > args.size() || myArguments.size() < args.size())
            throw new WrongNumberOfArgumentsException(myName.getName());

        Environment inner = new Environment(environment);
        List<LispSymbol> keys = new ArrayList<LispSymbol>(myArguments.keySet());
        if (!myArguments.isEmpty()) {
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LispObject argValue = args.get(i);
                LispSymbol argName =  keys.get(i);
                if (!myArguments.get(argName).equals("rest")) {
                    inner.setVariable(argName,argValue);
                    continue;
                }
                List<LispObject> rest = args.subList(i, argsSize);
                inner.setVariable(argName, new LispList(rest));
                for (int k = i+1; k!=keys.size(); ++k)
                    inner.setVariable(keys.get(k), LispSymbol.ourNil);
                break;
            }
            for (int k = args.size(); k!=keys.size(); ++k)
                inner.setVariable(keys.get(k), LispSymbol.ourNil);
        }

        LispObject result = LispSymbol.ourNil;
        for (int i=0; i!=myBody.size(); ++i) {
            result = myBody.get(i).evaluate(inner);
        }
        return result;
    }

    @Override
    public LispString getDefinition() {
        //TODO: differ from emacs functions
        //TODO: implement lambda
        //return toLambda();
        throw new NotImplementedException();  //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName.getName());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispCustomFunction that = (LispCustomFunction) o;

        if (myArguments != null ? !myArguments.equals(that.myArguments) : that.myArguments != null) return false;
        if (myBody != null ? !myBody.equals(that.myBody) : that.myBody != null) return false;
        if (myDocumentation != null ? !myDocumentation.equals(that.myDocumentation) : that.myDocumentation != null)
            return false;
        if (myInteractive != null ? !myInteractive.equals(that.myInteractive) : that.myInteractive != null)
            return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = myArguments != null ? myArguments.hashCode() : 0;
        result = 31 * result + (myDocumentation != null ? myDocumentation.hashCode() : 0);
        result = 31 * result + (myInteractive != null ? myInteractive.hashCode() : 0);
        result = 31 * result + (myBody != null ? myBody.hashCode() : 0);
        return result;
    }
}
