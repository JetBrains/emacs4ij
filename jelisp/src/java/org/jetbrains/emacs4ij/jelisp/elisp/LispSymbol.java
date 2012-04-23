package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.KeymapCell;
import org.jetbrains.emacs4ij.jelisp.exception.InternalException;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 7/11/11
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 *
 * elisp symbol = variable name, function name, constant name, special form name, etc
 */
public class LispSymbol implements LispAtom, LambdaOrSymbolWithFunction, KeymapCell {
    public static final LispSymbol ourNil = new LispSymbol("nil");
    public static final LispSymbol ourT = new LispSymbol("t");
    public static final LispSymbol ourVoid = new LispSymbol("void");

    private String myName = null;
    private LispObject myValue = null;
    private LispObject myFunction = null;
    private boolean isBufferLocal = false;
    private Map<LispSymbol, LispObject> myProperties = new HashMap<>();

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

    public LispSymbol (LispSymbol symbol, LispObject value) {
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

    public LispSymbol (String myName, LispObject value) {
        this.myName = myName;
        myValue = value;
    }

    public LispSymbol (String myName, LispObject value, boolean bufferLocal) {
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

    public LispObject getValue() {
        return myValue;
    }

    public void setValue(LispObject myValue) {
        this.myValue = myValue;
    }

    public void castToLambda () {
        if (isCustom() && !(myFunction instanceof Lambda)) {
            myFunction = new Lambda((LispList) myFunction);
        }
    }

    private void castToMacro () {
        if (isMacro() && !(myFunction instanceof LispMacro)) {
            myFunction = new LispMacro((LispList) myFunction);
        }
    }

    public LispObject getFunction () {
        return myFunction;
    }

    public void setFunction(LispObject function) {
        // this is for strict definition of this function
        if (myName.equals("exit-minibuffer") && myFunction != null)
            return;
        myFunction = function;
    }

    @Override
    public String toString() {
        return myName;
    }

    public boolean isSubroutine () {
        return myFunction instanceof Primitive;
    }

    public boolean isBuiltIn () {
        return isSubroutine() && ((Primitive) myFunction).getType().equals(Primitive.Type.BUILTIN);
    }
    
    private boolean isSpecialForm()  {
        return isSubroutine() && ((Primitive) myFunction).getType().equals(Primitive.Type.SPECIAL_FORM);
    }

    public boolean isCustom() {
        return ((myFunction instanceof LispList && ((LispList)myFunction).car().equals(new LispSymbol("lambda")))
                || myFunction instanceof Lambda);
    }

    public boolean isFunction() {
        return myFunction != null;
    }

    public boolean isMacro() {
        return ((myFunction instanceof LispList && ((LispList)myFunction).car().equals(new LispSymbol("macro")))
                || myFunction instanceof LispMacro);
    }
    
    public boolean isAlias() {
        return myFunction instanceof LispSymbol && ((LispSymbol) myFunction).isFunction();
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
    public LispObject evaluate(Environment environment) {
        if (equals(ourNil) || equals(ourT) || equals(ourVoid) || isKeyword())
            return this;
        if (myName.equals("obarray")) {
            return GlobalEnvironment.INSTANCE.getObjectArray();
        }
        if (hasValue()) {
            return getValue();
        }
        LispSymbol symbol = environment.find(myName);
        if (symbol == null || (!symbol.hasValue())) {
            System.out.println("VAR " + myName);
            symbol = environment.findAndRegisterEmacsVariable(myName);
            if (symbol == null || !symbol.hasValue()) {
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
            System.out.println(GlobalEnvironment.ourCallStack.toString());
            throw new InternalException(JelispBundle.message("callstack.error"));
        }
    }

    public LispObject evaluateFunction (Environment environment, @Nullable List<LispObject> args) {
        if (!environment.areSpecFormsAndMacroAllowed()) {
            if (isSpecialForm() || isMacro())
                throw new InvalidFunctionException(myFunction.toString());
            if (!isAlias())
                environment.setSpecFormsAndMacroAllowed(true);
        }

        GlobalEnvironment.ourCallStack.push(myName);
        
        LispObject result;
        if (args == null)
            args = new ArrayList<>();
        if (isSubroutine()) {
            result = LispSubroutine.evaluate(myName, environment, args);
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

    public LispObject macroExpand (Environment environment, List<LispObject> args) {
        castToMacro();
        try {
            return ((LispMacro)myFunction).expand(environment, args);
        } catch (ClassCastException e) {
            throw new InternalException(JelispBundle.message("wrong.macro", myName));
        }
    }

    private LispObject evaluateMacro(Environment environment, List<LispObject> args) {
        return macroExpand(environment, args).evaluate(environment);
    }

    private LispObject evaluateCustomFunction (Environment environment, List<LispObject> args) {
        if (!environment.areArgumentsEvaluated()) {
            for (int i = 0, dataSize = args.size(); i < dataSize; i++) {
                args.set(i, args.get(i).evaluate(environment));
            }
        } else {
            environment.setArgumentsEvaluated(false);
        }
        castToLambda();
        return ((Lambda)myFunction).evaluate(environment, args);
    }

    public LispList getPropertyList() {
        List<LispObject> pList = new ArrayList<>();
        for (Map.Entry<LispSymbol, LispObject> entry: myProperties.entrySet()) {
            pList.add(entry.getKey());
            pList.add(entry.getValue());
        }
        return LispList.list(pList);
    }

    public LispObject getProperty (String pName) {
        LispObject value = getProperty(new LispSymbol(pName));
        return Core.thisOrNil(value);
    }

    public LispObject getProperty(LispSymbol pName) {
        return myProperties.get(pName);
    }

    public void setProperty(LispSymbol key, LispObject value) {
        myProperties.put(key, value);
    }

    public void setProperty(String keyName, LispObject value) {
        myProperties.put(new LispSymbol(keyName), value);
    }

    private boolean castFunctionCell () {
        if (myFunction instanceof FunctionCell)
            return true;
        if (isCustom()) {
            castToLambda();
            return true;
        } else if (isMacro()) {
            castToMacro();
            return true;
        }
        return false;
    }

    public LispObject getDocumentation () {
        if (myFunction == null)
            return getProperty(JelispBundle.message("var.doc"));
        castFunctionCell();
        return ((FunctionCell)myFunction).getDocumentation();
    }

    public void setVariableDocumentation (LispObject value) {
        if (value instanceof LispString)
            setProperty(JelispBundle.message("var.doc"), value);
    }
    
    public void setFunctionDocumentation (LispObject doc) {
        if (myFunction == null)
            return;
        castFunctionCell();
        ((FunctionCell) myFunction).setDocumentation(doc);
    }

    public void setGlobalVariableDocumentation (LispObject value) {
        setProperty(JelispBundle.message("var.doc"), value);
    }

    public boolean isKeyword () {
        return myName.startsWith(":");
    }

    @Override
    public boolean isInteractive() {
        if (myFunction == null)
            return false;
        castFunctionCell();
        return myFunction instanceof LispCommand && ((LispCommand) myFunction).isInteractive();
    }

    @Override
    public String getInteractiveString() {
        if (myFunction == null)
            return null;
        castFunctionCell();
        if (myFunction instanceof LispCommand)
            return ((LispCommand)myFunction).getInteractiveString();
        return null;
    }

    @Override
    public LispList getInteractiveForm() {
        if (myFunction == null)
            return null;
        castFunctionCell();
        if (myFunction instanceof LispCommand)
            return ((LispCommand)myFunction).getInteractiveForm();
        return null;
    }

    @Override
    public LispKeymap getKeymap() {
        LispSymbol symbol = this;
        while (symbol.isFunction()) {
            LispObject function = symbol.getFunction();
            if (function instanceof LispKeymap)
                return (LispKeymap) function;
            if (function instanceof LispSymbol) {
                symbol = (LispSymbol) function;
                continue;
            }
            return null;
        }
        return null;
    }
}
