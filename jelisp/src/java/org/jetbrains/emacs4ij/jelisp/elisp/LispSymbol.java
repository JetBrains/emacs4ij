package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

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

    private String myName = null;
    private LObject myValue = null; //ourVoid;
    private LObject myFunction = null;
    private boolean isBufferLocal = false;
    private HashMap<LispSymbol, LObject> myProperties = new HashMap<LispSymbol, LObject>();

    public LispSymbol(String myName) {
        this.myName = myName;
    }
    
    public LispSymbol (LispSymbol symbol) {
        myName = symbol.myName;
        myValue = symbol.myValue;
        myFunction = symbol.myFunction;
        isBufferLocal = symbol.isBufferLocal;
        myProperties = symbol.myProperties;
    }

    public LispSymbol (LispSymbol symbol, LObject value) {
        myName = symbol.myName;
        myValue = value;
        myFunction = symbol.myFunction;
        isBufferLocal = symbol.isBufferLocal;
        myProperties = symbol.myProperties;
    }
    
    public static LispSymbol bool (boolean value) {
        if (value)
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }
    
    public boolean toBoolean () {
        return !this.equals(ourNil);
    }

    public LispSymbol(String myName, boolean bufferLocal) {
        this.myName = myName;
        isBufferLocal = bufferLocal;
    }

    public LispSymbol (String myName, LObject value) {
        this.myName = myName;
        myValue = value;
    }

    public LispSymbol (String myName, LObject value, boolean bufferLocal) {
        this.myName = myName;
        myValue = value;
        isBufferLocal = bufferLocal;
    }

    public boolean isBufferLocal() {
        return isBufferLocal;
    }
    
    public void setBufferLocal (boolean bufferLocal) {
        isBufferLocal = bufferLocal;
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

    private void castToLambda (Environment environment) {
        if (isCustom() && !(myFunction instanceof Lambda)) {
            myFunction = new Lambda((LispList) myFunction, environment);
        }
    }

    private void castToMacro (Environment environment) {
        if (isMacro() && !(myFunction instanceof Macro)) {
            myFunction = new Macro((LispList) myFunction, environment);
        }
    }

    public LObject getFunction () {
        return myFunction;
    }

    public void setFunction(LObject myFunction) {
        this.myFunction = myFunction;
    }

    @Override
    public String toString() {        
        if (myFunction == null) {
            return myName;
//            return myName + " = " + (myValue == null ? "null" : myValue.toString());
        }
        return myFunction.toString();
//        return myName + " = " + myFunction.toString();
    }

    public boolean isSubroutine () {
        return myFunction instanceof Primitive;
    }

    public boolean isBuiltIn () {
        return isSubroutine() && ((Primitive) myFunction).getType().equals(Primitive.Type.BUILTIN);
    }

    public boolean isCustom() {
        return ((myFunction instanceof LispList && ((LispList)myFunction).car().equals(new LispSymbol("lambda")))
                || (myFunction instanceof Lambda));
    }

    public boolean isFunction() {
        return myFunction != null;
    }

    public boolean isMacro() {
        return (myFunction instanceof LispList && ((LispList)myFunction).car().equals(new LispSymbol("macro"))
                || (myFunction instanceof Macro));
    }
    
    public boolean isAlias() {
        return myFunction instanceof LispSymbol && ((LispSymbol) myFunction).isFunction();
    }

    public boolean isInteractive (Environment environment) {
        if (!isFunction())
            throw new RuntimeException("wrong usage of function isInteractive with symbol " + myName +
                ", functionCell = " + (myFunction == null ? "NULL" : myFunction.toString()));

        castFunctionCell(environment);
        return ((FunctionCell)myFunction).isInteractive();
        /*if (isCustom()) {
            castToLambda(environment);
            return ((Lambda)myFunction).isInteractive();
        } if (isSubroutine()) {
            return ((Primitive)myFunction).isInteractive();
        }
        throw new RuntimeException("wrong usage of function isInteractive with symbol " + myName +
                ", functionCell = " + (myFunction == null ? "NULL" : myFunction.toString()));     */
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null) return false;

        if (o.getClass().equals(LispList.class) && ((LispList)o).isEmpty() && this.equals(ourNil))
            return true;
        if (o.getClass() != getClass())
            return false;

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

    public boolean hasValue () {
        return myValue != null;
    }

    @Override
    /**
     * takes Environment
     */
    public LObject evaluate(Environment environment) {
        if (equals(ourNil) || equals(ourT) || equals(ourVoid) || isKeyword())
            return this;
        if (hasValue()) {
            return getValue();
        }
        LispSymbol symbol = environment.find(myName);
        if (symbol == null || (!symbol.hasValue())) {
            System.out.println("VAR " + myName);
            try {
                symbol = GlobalEnvironment.INSTANCE.findAndRegisterEmacsForm(myName, GlobalEnvironment.SymbolType.VAR);
            } catch (Exception e) {
                //throw new VoidVariableException(myName);
                throw e;
            }
            if (symbol == null || (!symbol.hasValue())) {
                System.err.println("void-var '" + myName + "'");
                throw new VoidVariableException(myName);
            }
        }
        return symbol.getValue();
    }

    private void checkCallStack () {
        String q = GlobalEnvironment.ourCallStack.removeFirst();
        if (myName.equals("catch"))
            while (!q.equals(myName))
                q = GlobalEnvironment.ourCallStack.removeFirst();
        if (!q.equals(myName)) {
            throw new RuntimeException("bug in call stack");
        }
    }

    public LObject evaluateFunction (Environment environment, @Nullable List<LObject> args) {
        GlobalEnvironment.ourCallStack.push(myName);
        LObject result;
        if (args == null)
            args = new ArrayList<>();
        if (isSubroutine()) {
            result = LispSubroutine.evaluate(this, environment, args);
            checkCallStack();
            return result;
        }
        if (isMacro()) {
            result = evaluateMacro(environment, args);
            checkCallStack();
            return result;
        }
        if (isAlias()) {
            result = ((LispSymbol)myFunction).evaluateFunction(environment, args);
            checkCallStack();
            return result;
        }
        result = evaluateCustomFunction(environment, args);
        checkCallStack();
        return result;
    }

    public LObject macroExpand (Environment environment, List<LObject> args) {
        castToMacro(environment);
        try {
            return ((Macro)myFunction).expand(environment, args);
        } catch (ClassCastException e) {
            throw new RuntimeException("Wrong cast to macro: " + myName);
        }
    }

    private LObject evaluateMacro(Environment environment, List<LObject> args) {
        return macroExpand(environment, args).evaluate(environment);
    }

    private LObject evaluateCustomFunction (Environment environment, List<LObject> args) {
        if (!environment.areArgumentsEvaluated()) {
            for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                args.set(i, args.get(i).evaluate(environment));
            }
        } else {
            environment.setArgumentsEvaluated(false);
        }
        castToLambda(environment);
        return ((Lambda)myFunction).evaluate(environment, args);
    }

    public LispObject getPropertyList() {
        ArrayList<LObject> pList = new ArrayList<>();
        for (LispSymbol key: myProperties.keySet())
            pList.add(LispList.list(key, myProperties.get(key)));
        return LispList.list(pList);
    }

    public LObject getProperty (String pName) {
        return getProperty(new LispSymbol(pName));
    }

    public LObject getProperty(LispSymbol pName) {
        if (myProperties.containsKey(pName))
            return myProperties.get(pName);
        return LispSymbol.ourNil;
    }

    public void setProperty(LispSymbol key, LObject value) {
        myProperties.put(key, value);
    }

    public void setProperty(String keyName, LObject value) {
        myProperties.put(new LispSymbol(keyName), value);
    }

    private void castFunctionCell (Environment environment) {
        if (isCustom()) {
            castToLambda(environment);
        } else if (isMacro()) {
            castToMacro(environment);
        }
    }

    public LObject getDocumentation (Environment environment) {
        if (myFunction == null)
            return getProperty("variable-documentation");
        castFunctionCell(environment);
        return ((FunctionCell)myFunction).getDocumentation();
    }

    public void setVariableDocumentation (LObject value) {
        if (value instanceof LispString)
            setProperty("variable-documentation", value);
    }
    
    public void setFunctionDocumentation (LObject doc, Environment environment) {
        if (myFunction == null)
            return;
        castFunctionCell(environment);
        ((FunctionCell) myFunction).setDocumentation(doc);
    }

    public void setGlobalVariableDocumentation (LObject value) {
        setProperty("variable-documentation", value);
    }

    public String getInteractiveString (Environment environment) {
        if (myFunction == null)
            return null;
        castFunctionCell(environment);
        return ((FunctionCell)myFunction).getInteractiveString();
    }

    public boolean isKeyword () {
        return myName.startsWith(":");
    }
}
