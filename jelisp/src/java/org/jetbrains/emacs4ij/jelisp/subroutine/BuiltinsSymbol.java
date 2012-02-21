package org.jetbrains.emacs4ij.jelisp.subroutine;

import org.jetbrains.emacs4ij.jelisp.Environment;
import org.jetbrains.emacs4ij.jelisp.GlobalEnvironment;
import org.jetbrains.emacs4ij.jelisp.elisp.*;
import org.jetbrains.emacs4ij.jelisp.exception.InvalidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidFunctionException;
import org.jetbrains.emacs4ij.jelisp.exception.VoidVariableException;

import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * Created by IntelliJ IDEA.
 * User: Ekaterina.Polishchuk
 * Date: 8/2/11
 * Time: 5:05 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class BuiltinsSymbol {
    private BuiltinsSymbol() {}

    @Subroutine("symbol-function")
    public static LObject symbolFunction(Environment environment, LispSymbol arg) {
        LispSymbol f = environment.find(arg.getName());
        if (f == null || !f.isFunction())
            throw new VoidFunctionException(arg.getName());
        return f.getFunction();
    }

    @Subroutine("symbol-value")
    public static LObject symbolValue (Environment environment, LispSymbol arg) {
        LispSymbol symbol = environment.find(arg.getName());
        if (symbol == null || symbol.getValue() == null || symbol.getValue().equals(LispSymbol.ourVoid))
            throw new VoidVariableException(arg.getName());
        return symbol.getValue();
    }

    @Subroutine("symbol-name")
    public static LObject symbolName (LispSymbol arg) {
        return new LispString(arg.getName());
    }

    @Subroutine("get")
    public static LObject get(Environment environment, LispSymbol symbol, LispSymbol propertyName) {
        LObject result = environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, propertyName);
        return result == null ? LispSymbol.ourNil : result;
    }

    @Subroutine("put")
    public static LObject put(Environment environment, LispSymbol symbol, LispSymbol propertyName, LObject value) {
        LObject found = environment.find(symbol.getName(), "setProperty", new Class[] {LispSymbol.class, LObject.class}, propertyName, value);
        if (found == null) {
            symbol.setProperty(propertyName, value);
            environment.defineSymbol(symbol);
        }
        return value;
    }

    @Subroutine("documentation-property")
    public static LObject documentationProperty (Environment environment, LispSymbol symbol, LispSymbol propertyName, @Optional LObject verbatim)  {
        //todo: if (verbatim != null) && !(verbatim.equals(LispSymbol.ourNil) ---
        // Third argument RAW omitted or nil means pass the result through `substitute-command-keys' if it is a string.
        LObject value = environment.find(symbol.getName(), "getProperty", new Class[]{LispSymbol.class}, propertyName);
        if (!(value instanceof LispString)) {
            if (!(value instanceof LispInteger))
                return value.evaluate(environment);

            int offset = ((LispInteger) value).getData();
            if (offset < 0)
                offset = -offset;
            try {
                RandomAccessFile docFile = new RandomAccessFile(((LispString)GlobalEnvironment.INSTANCE.find("doc-directory").getValue()).getData() +
                        ((LispString)GlobalEnvironment.INSTANCE.find("internal-doc-file-name").getValue()).getData(), "r");
                docFile.seek(offset);
                String doc = "";
                String line = docFile.readLine();
                while (line != null && !line.contains("")) {
                    doc += (doc.length() > 0 ? '\n' : "") + line;
                    line = docFile.readLine();
                }
                docFile.close();
                if (line != null)
                    doc += (doc.length() > 0 ? '\n' : "") + line.substring(0, line.indexOf(''));
                return new LispString(doc);
            } catch (IOException e) {
                //throw new RuntimeException(e.getMessage());
                return LispSymbol.ourNil;
            }
        }
        return value;
    }

    @Subroutine("documentation")
    public static LObject documentation (Environment environment, LispObject function) {
        if (function instanceof LispSymbol) {
            String name = ((LispSymbol) function).getName();
            LispSymbol f = environment.find(name);
            if (f == null)
                throw new VoidFunctionException(name);

            LObject funPropValue = documentationProperty(environment, f, new LispSymbol("function-documentation"), null);
            if (!funPropValue.equals(LispSymbol.ourNil))
                return funPropValue;

            if (!f.isFunction())
                throw new VoidFunctionException(name);
            return f.getDocumentation(environment);

        } else if (function instanceof Lambda) {
            return ((Lambda) function).getDocumentation();
        } else if (function instanceof Macro) {
            return ((Macro)function).getDocumentation();
        } else if (function instanceof Primitive) {
            return ((Primitive) function).getDocumentation();
        }
        throw new InvalidFunctionException(function.toString());
    }


    @Subroutine(value = "default-value")
    public static LObject defaultValue (Environment environment, LispSymbol symbol) {
        LispSymbol real = environment.find(symbol.getName());
        if (real == null)// || !real.hasValue())
            throw new VoidVariableException(symbol.getName());
        if (!real.isBufferLocal()) {
            if (!real.hasValue())
                throw new VoidVariableException(symbol.getName());
            return real.getValue();
        }
        LObject value = GlobalEnvironment.INSTANCE.getBufferLocalSymbolValue(symbol);
        if (value == null)
            throw new VoidVariableException(symbol.getName());
        return value;
    }

    @Subroutine("set-default")
    public static LObject setDefault (Environment environment, LispSymbol symbol, LObject value) {
        LispSymbol real = GlobalEnvironment.INSTANCE.find(symbol.getName());
        if (real != null) {
            //todo: check for buffer-locality
           // real.setBufferLocal(true); //?
            real.setValue(value);
        } else
            symbol.setValue(value);
            GlobalEnvironment.INSTANCE.defineSymbol(new LispSymbol(symbol));
        return value;
    }
    
    @Subroutine(value = "make-variable-buffer-local", isCmd = true, interactive = "")
    public static LObject makeVariableBufferLocal (Environment environment, LispSymbol variable) {
        if (!(environment instanceof GlobalEnvironment)) {
            LObject var = environment.find(variable.getName());           
            if (var == null)
                environment.defineSymbol(variable);           
        } else {
            ((GlobalEnvironment) environment).defineBufferLocalVariable(variable);
        }
        return variable;
    }
    
    @Subroutine("make-symbol")
    public static LObject makeSymbol (LispString name) {
        return new LispSymbol(name.getData());

    }
}
