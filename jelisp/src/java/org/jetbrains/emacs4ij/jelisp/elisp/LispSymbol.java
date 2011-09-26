package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.exception.WrongNumberOfArgumentsException;

import java.util.*;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp symbol = variable name, function name, constant name, special form name, etc
 */
public class LispSymbol extends LispAtom {
    public static final LispSymbol ourNil = new LispSymbol("nil");
    public static final LispSymbol ourT = new LispSymbol("t");
    public static final LispSymbol ourVoid = new LispSymbol("void");

    public enum FunctionType {SpecialForm ("special-form"), BuiltIn ("built-in"), Custom ("custom");
        private final String myValue;
        FunctionType (String value) {
            myValue = value;
        }
        public String getValue() {
            return myValue;
        }
    }

    private String myName = null;
    private LObject myValue = ourVoid;
    private LispObject myFunction = ourVoid;
    private HashMap<LispSymbol, LispObject> myProperties = new HashMap<LispSymbol, LispObject>();


    public LispSymbol(String myName) {
        this.myName = myName;
    }

    public LispSymbol (String myName, FunctionType functionType) {
        this.myName = myName;
        if (functionType.equals(FunctionType.BuiltIn) || functionType.equals(FunctionType.SpecialForm)) {
            myFunction = new LispString("#<subr " + myName + ">");
            setProperty("function-type", new LispString(functionType.getValue()));
            return;
        }
        throw new RuntimeException("Invalid initialization of custom function " + myName + ", type " + functionType.getValue());
    }

    public LispSymbol (String myName, LObject value) {
        this.myName = myName;
        myValue = value;
    }

    public String getName() {
        return myName;
    }

    public LObject getValue() {
        return myValue;
    }

    public void setValue(LObject myValue) {
        this.myValue = myValue;
    }

    public LispObject getFunction() {
        return myFunction;
    }

    public void setFunction(LispObject myFunction) {
        setProperty("function-type", new LispString(FunctionType.Custom.getValue()));
        this.myFunction = myFunction;
    }

    @Override
    public String toString() {
        return "LispSymbol{" +
                "myName='" + myName + '\'' +
                '}';
    }

    public boolean is (FunctionType functionType) {
        LispObject fType = getProperty("function-type");
        return fType.equals(new LispString(functionType.getValue()));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LispSymbol that = (LispSymbol) o;

        return !(myName != null ? !myName.equals(that.myName) : that.myName != null);

    }

    @Override
    public int hashCode() {
        int result = myName != null ? myName.hashCode() : 0;
        result = 31 * result + (myValue != null ? myValue.hashCode() : 0);
        result = 31 * result + (myFunction != null ? myFunction.hashCode() : 0);
        result = 31 * result + (myProperties != null ? myProperties.hashCode() : 0);
        return result;
    }

    @Override
    public LispString toLispString() {
        return new LispString(myName);
    }

    @Override
    /**
     * takes Environment
     */
    public LObject evaluate(Environment environment) {
        LObject lispObject = environment.find(myName, "getValue");
        if (lispObject == null || lispObject.equals(LispSymbol.ourVoid))
            throw new VoidVariableException(myName);
        return lispObject;
    }

    public LObject evaluateFunction (Environment environment, List<LObject> args) {

        if (is(FunctionType.BuiltIn) || is(FunctionType.SpecialForm))
            return LispSubroutine.evaluate(this, environment, args);

        //custom function
        for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
            args.set(i, args.get(i).evaluate(environment));
        }

        List<LObject> functionData = ((LispList)myFunction).getData();
        if (!functionData.get(0).equals(new LispSymbol("lambda")))
            throw new RuntimeException("unsupported custom function " + functionData.get(0).toString());
        LinkedHashMap<LispSymbol, String> arguments = readMyArguments((LispList) functionData.get(1));
        Environment inner = substituteArguments(environment, args, arguments);
        LispString docString = null;

        try {
            int index = 2;
            if (functionData.get(index) instanceof LispString) {
                docString = (LispString) functionData.get(index);
                ++index;
            }
            if (functionData.get(index) instanceof LispList) {
                if (((LispList)functionData.get(index)).car().equals(new LispSymbol("interactive")))
                    ++index;
            }
            LObject result = LispSymbol.ourNil;
            for (; index != functionData.size(); ++index) {
                result = functionData.get(index).evaluate(inner);
            }
            return result;

        } catch (IndexOutOfBoundsException e) {
            return (docString == null) ? LispSymbol.ourNil : docString;
        }
    }

    private Environment substituteArguments (Environment environment, List<LObject> args, LinkedHashMap<LispSymbol, String> arguments) {
        int nRequiredArguments = 0;
        for (Map.Entry<LispSymbol, String> arg: arguments.entrySet()) {
            if (arg.getValue().equals("required"))
                ++nRequiredArguments;
        }
        if (nRequiredArguments > args.size() || arguments.size() < args.size())
            throw new WrongNumberOfArgumentsException(myName);

        Environment inner = new Environment(environment);
        List<LispSymbol> keys = new ArrayList<LispSymbol>(arguments.keySet());
        if (!arguments.isEmpty()) {
            for (int i = 0, argsSize = args.size(); i < argsSize; i++) {
                LObject argValue = args.get(i);
                LispSymbol argName =  keys.get(i);
                if (!arguments.get(argName).equals("rest")) {
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

    private LinkedHashMap<LispSymbol, String> readMyArguments (LispList args) {
        LinkedHashMap<LispSymbol, String> arguments = new LinkedHashMap<LispSymbol, String>();
        if (args.isEmpty())
            return arguments;

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
            arguments.put(argName, type);
        }
        return arguments;
    }

    public LispObject getPropertyList() {
        LispList pList = new LispList();
        for (LispSymbol key: myProperties.keySet())
            pList.add(new LispList(key, myProperties.get(key)));
        return pList;
    }

    public LispObject getProperty (String pName) {
        return getProperty(new LispSymbol(pName));
    }

    public LispObject getProperty(LispSymbol pName) {
        if (myProperties.containsKey(pName))
            return myProperties.get(pName);
        return LispSymbol.ourNil;
    }

    public void setProperty(LispSymbol key, LispObject value) {
        myProperties.put(key, value);
    }

    public void setProperty(String keyName, LispObject value) {
        myProperties.put(new LispSymbol(keyName), value);
    }

    public LispObject getVariableDocumentation () {
        return getProperty("variable-documentation");
    }

    public void setVariableDocumentation (LispString value) {
        setProperty("variable-documentation", value);
    }
}
