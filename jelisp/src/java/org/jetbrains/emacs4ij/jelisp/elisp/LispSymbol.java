package org.jetbrains.emacs4ij.jelisp.elisp;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.JelispBundle;
import org.jetbrains.emacs4ij.jelisp.exception.*;
import org.jetbrains.emacs4ij.jelisp.platformDependent.LispKeymap;
import org.jetbrains.emacs4ij.jelisp.subroutine.Core;
import org.jetbrains.emacs4ij.jelisp.subroutine.Predicate;
import org.jetbrains.emacs4ij.jelisp.subroutine.Symbol;

import java.lang.reflect.InvocationTargetException;
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
public final class LispSymbol implements LispAtom, LambdaOrSymbolWithFunction, KeymapCell {
    public static final LispSymbol ourNil = new LispSymbol("nil");
    public static final LispSymbol ourT = new LispSymbol("t");
    public static final LispSymbol ourVoid = new LispSymbol("void");

    private String myName = null;
    private Map<LispSymbol, LispObject> myProperties = new HashMap<>();
    private LispObject myValue = null;
    private boolean isAlias = false;

    private List<LispSymbol> myAliases = null;
    private LispObject myFunction = null;
    private boolean isBufferLocal = false;
    private boolean isConstant = false;

    public LispSymbol(String name) {
        myName = name;
    }

    public LispSymbol(boolean constant, String name) {
        isConstant = constant;
        myName = name;
    }
    
    public LispSymbol (LispSymbol symbol) {
        myName = symbol.myName;
        myValue = symbol.myValue;
        myFunction = symbol.myFunction;
        isBufferLocal = symbol.isBufferLocal;
        myProperties = symbol.myProperties;
        isConstant = symbol.isConstant;
        isAlias = symbol.isAlias;
        myAliases = symbol.myAliases;
    }

    public LispSymbol (LispSymbol symbol, LispObject value) {
        myName = symbol.myName;
        myValue = value;
        myFunction = symbol.myFunction;
        isBufferLocal = symbol.isBufferLocal;
        myProperties = symbol.myProperties;
        isConstant = symbol.isConstant;
        isAlias = symbol.isAlias;
        myAliases = symbol.myAliases;
    }

    public static LispSymbol bool (boolean value) {
        if (value)
            return LispSymbol.ourT;
        return LispSymbol.ourNil;
    }
    
    public boolean toBoolean () {
        return !this.equals(ourNil);
    }

    public LispSymbol(String name, boolean bufferLocal) {
        myName = name;
        isBufferLocal = bufferLocal;
    }

    public LispSymbol (String name, LispObject value) {
        myName = name;
        myValue = value;
    }

    public LispSymbol (boolean constant, String name, LispObject value) {
        isConstant = constant;
        myName = name;
        myValue = value;
    }

    public LispSymbol (String name, LispObject value, boolean bufferLocal) {
        myName = name;
        myValue = value;
        isBufferLocal = bufferLocal;
    }

    public boolean isBufferLocal() {
        return isBufferLocal;
    }
    
    public void setBufferLocal (boolean bufferLocal) {
        isBufferLocal = bufferLocal;
    }

    public void setConstant() {
        isConstant = true;
    }
    
    public String getName() {
        return myName;
    }

    public LispObject getValue() {
        if (!isAlias)
            return myValue;
        LispSymbol symbol = this;
        String met = "";
        while (symbol.isAlias) {
            if (met.contains(symbol.getName() + ";"))
                throw new CyclicVariableIndirectionException(this);
            met += symbol.getName() + ";";
            symbol = (LispSymbol)symbol.myValue;
        }
        return symbol.getValue();
    }

    public void setValue(LispObject value) {
        if (isConstant)
            throw new SetConstException(myName);
        if (!isAlias) {
            myValue = value;
            return;
        }
        LispSymbol symbol = this;
        String met = "";
        while (symbol.isAlias) {
            if (met.contains(symbol.getName() + ";"))
                throw new CyclicVariableIndirectionException(this);
            met += symbol.getName() + ";";
            symbol = (LispSymbol)symbol.myValue;
        }
        symbol.setValue(value);
    }

    public void setAsAlias(LispSymbol value) {
        if (isConstant)
            throw new SetConstException(myName);
        isAlias = true;
        myValue = value;
        isBufferLocal = value.isBufferLocal();
        value.addAlias(this);
    }

    protected void addAlias(LispSymbol alias) {
        if (myAliases == null)
            myAliases = new ArrayList<>();
        if (myAliases.contains(alias))
            return;
        myAliases.add(alias);
    }

    @Nullable
    public List<LispSymbol> getAliases() {
        return myAliases;
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
        // this is for strict definition of exit-minibuffer
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
    
    public boolean isFunctionAlias() {
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
//        result = 31 * result + (myValue != null ? myValue.hashCode() : 0);
//        result = 31 * result + (myFunction != null ? myFunction.hashCode() : 0);
//        result = 31 * result + (myProperties != null ? myProperties.hashCode() : 0);
        return result;
    }

    public boolean hasValue () {
        return myValue != null;
    }

    public boolean isKeyword() {
        return myName.startsWith(":");
    }

    @NotNull
    @Override
    /**
     * takes Environment
     */
    public LispObject evaluate(Environment environment) {
        if (isConstant)
            return this;
        if (myName.equals("obarray")) {
            return GlobalEnvironment.INSTANCE.getObjectArray();
        }

        LispSymbol symbol = environment.find(myName);
        if (symbol == null && isKeyword())
            return this;
        if (symbol != null && symbol.isConstant)
            return symbol;

        if (symbol == null || !symbol.hasValue()) {
            //System.out.println("VAR " + myName);
            try {
                symbol = environment.findAndRegisterEmacsVariable(myName);
            } catch (CyclicDefinitionLoadException e) {
                throw new VoidVariableException(myName);
            }
            if (symbol == null || !symbol.hasValue()) {
                throw new VoidVariableException(myName);
            }
        }
        LispObject value = symbol.getValue();
        if (value == null) //it was alias and the alias root is not set
            throw new VoidVariableException(myName);
        return value;
    }

    @NotNull
    public LispSymbol uploadVariableDefinition() {
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(myName);
        if (symbol == null && isKeyword())
            return this;
        if (symbol == null) {
            //System.out.println("VAR " + myName);
            symbol =  GlobalEnvironment.INSTANCE.findAndRegisterEmacsVariable(myName);
        }
        return symbol;
    }

    private void checkCallStack () {
        String q = GlobalEnvironment.ourCallStack.removeFirst();
        if (myName.equals("catch"))
            while (!q.equals(myName))
                q = GlobalEnvironment.ourCallStack.removeFirst();
        if (!q.equals(myName)) {
            if (myName.equals("eval-last-sexp")) {
                //we caught exception during the evaluation, so clear call stack
                assert GlobalEnvironment.ourCallStack.getLast().equals(myName);
                GlobalEnvironment.ourCallStack.clear();
                return;
            }
            System.out.println(String.format("Top of stack = %s, current symbol = %s, left stack = %s",
                    q, myName, GlobalEnvironment.ourCallStack.toString()));
            throw new InternalException(JelispBundle.message("call.stack.error"));
        }
    }

    public LispSymbol uploadFunctionDefinition (Environment environment, Class exception) {
        if (isFunction())
            return this;
        LispSymbol symbol = GlobalEnvironment.INSTANCE.find(myName);
        if (symbol == null || !symbol.isFunction()) {
            //while we are not loading all emacs lisp code, perform search on request
            //System.out.println("FUN " + myName);
            try {
                symbol = environment.findAndRegisterEmacsFunction(myName);
            } catch (CyclicDefinitionLoadException e) {
                throw new CyclicFunctionIndirectionException(myName);
            } catch (LispException e) {
                throw new VoidFunctionException(myName);
            }
            if (symbol == null || !symbol.isFunction()) {
                try {
                    throw (LispException) exception.getDeclaredConstructor(String.class).newInstance(myName);
                } catch (NoSuchMethodException | InvocationTargetException | InstantiationException |IllegalAccessException e) {
                    e.printStackTrace();
                    throw new LispException(e.getMessage());
                }
            }
        }
        return symbol;
    }

    public LispObject evaluateFunction (Environment environment, Class exception, @Nullable List<LispObject> args) {
        LispSymbol trueFunction = uploadFunctionDefinition(environment, exception);
        return trueFunction.evaluateTrueFunction(environment, exception, args);
    }

    private LispObject evaluateTrueFunction(Environment environment, Class exception, @Nullable List<LispObject> args) {
        GlobalEnvironment.ourCallStack.push(myName);
        return eval(environment, exception, args);
    }

    private LispObject eval (Environment environment, Class exception, @Nullable List<LispObject> args) {
        if (!environment.areSpecFormsAndMacroAllowed()) {
            if (isSpecialForm() || isMacro())
                throw new InvalidFunctionException(myName);
            if (!isFunctionAlias())
                environment.setSpecFormsAndMacroAllowed(true);
        }
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
        if (isFunctionAlias()) {
            result = ((LispSymbol)myFunction).evaluateFunction(environment, exception, args);
            checkCallStack();
            return result;
        }
        if (isAutoload()) {
            uploadAutoload();
            return eval(environment, exception, args);
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

    private void uploadAutoload() {
        String fileName = ((LispString) getCarIfList(((LispList)myFunction).cdr())).getData();
        LispObject type;
        try {
            type = getCarIfList(((LispList)myFunction).nthCdr(4));
        } catch (LispException e) {
            type = LispSymbol.ourNil;
        }
        try {
            LispList def = GlobalEnvironment.INSTANCE.getEmacsDefFromFile(myName, fileName, type);
            if (def == null)
                throw new AutoloadDefFromFileException(fileName, myName);
            def.evaluate(GlobalEnvironment.INSTANCE);
            castFunctionCell();
        } catch (ReadException e) {
            throw new AutoloadDefFromFileException(fileName, myName);
        }
    }

    private void castFunctionCell () {
        if (myFunction instanceof FunctionCell)
            return;
        if (isCustom()) {
            castToLambda();
            return;
        }
        if (isMacro()) {
            castToMacro();
            return;
        }
        if (isAutoload()) {
            uploadAutoload();
            return;
        }
        throw new InvalidFunctionException(myFunction.toString());
    }

    private boolean isAutoload() {
        return myFunction instanceof LispList && ((LispList) myFunction).car().equals(new LispSymbol("autoload"))
                && getCarIfList(((LispList)myFunction).cdr()) instanceof LispString;
    }

    private LispObject getCarIfList (LispObject object) {
        return object instanceof LispList ? ((LispList) object).car() : object;
    }

    public LispObject getDocumentation () {
        if (myFunction == null)
            return getProperty(JelispBundle.message("var.doc"));
        if (isAutoload()) {
            LispObject doc = LispSymbol.ourNil;
            try {
                doc = getCarIfList(((LispList)myFunction).nthCdr(2));
            } catch (LispException e) {
                //skip: autoload form may have no documentation
            }
            return Symbol.getDocumentationProperty(GlobalEnvironment.INSTANCE, doc);
        }

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
        if (myFunction instanceof LispSymbol) {
            setProperty("function-documentation", doc);
            return;
        }
        castFunctionCell();
        ((FunctionCell) myFunction).setDocumentation(doc);
    }

    public void setGlobalVariableDocumentation (LispObject value) {
        setProperty(JelispBundle.message("var.doc"), value);
    }

    @Override
    public boolean isInteractive() {
        if (myFunction == null)
            return false;
        if (isAutoload()) {
            try {
                LispObject interactive = getCarIfList(((LispList)myFunction).nthCdr(4));
                return !Predicate.isNil(interactive);
            } catch (LispException e) {
                //autoload form may have no interactive form
                return false;
            }
        }
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
        //todo: upload keymap if autoload
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

    //for variable alias -- get all variables in alias chain, step by step
    public LispSymbol next() {
        return isAlias ? (LispSymbol) myValue : null;
    }

    //returns this or root symbol for variable alias
    public LispSymbol getSource() {
        if (myValue == null || !(myValue instanceof LispSymbol) || !isAlias)
            return this;
        return ((LispSymbol)myValue).getSource();
    }

}
